# Paquetes
library(arrow)
library(dplyr)
library(ggplot2)
library(knitr)
library(purrr)
library(scales)
library(tibble)
library(tidyr)

# Configuracion del caso
hospital_name <- "Complejo Hospitalario Dr. Sótero del Río (Santiago, Puente Alto)"
target_weeks <- 20:27
analysis_years <- 2022:2024
admission_rate <- 0.18
base_capacity <- 40
base_occupancy <- 28
weibull_shape <- 1.6
weibull_scale <- 5.8
horizon_days <- length(target_weeks) * 7
n_sim <- 1000

# Carga y preparacion de datos
data_candidates <- c(
  file.path("clases", "data", "at_urg_respiratorio_semanal.parquet"),
  file.path("data", "at_urg_respiratorio_semanal.parquet")
)

data_path <- data_candidates[file.exists(data_candidates)][1]
if (is.na(data_path)) {
  stop("No se encontró 'at_urg_respiratorio_semanal.parquet' en una ruta utilizable.")
}

urgencias <- read_parquet(data_path) |>
  filter(
    EstablecimientoGlosa == hospital_name,
    Causa == "TOTAL CAUSAS SISTEMA RESPIRATORIO"
  ) |>
  arrange(Anio, SemanaEstadistica)

demanda_base <- urgencias |>
  filter(Anio %in% analysis_years, SemanaEstadistica %in% target_weeks) |>
  group_by(SemanaEstadistica) |>
  summarise(
    urgencias_promedio = mean(NumTotal, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    ingresos_base = urgencias_promedio * admission_rate,
    ingresos_alta_demanda = ingresos_base * 1.20
  )

stopifnot(nrow(demanda_base) == length(target_weeks))

daily_lambda_from_weekly <- function(weekly_admissions) {
  rep(weekly_admissions / 7, each = 7)
}

simulate_occupancy <- function(
  daily_lambda,
  capacity,
  los_shape,
  los_scale,
  baseline_occupancy,
  n_sim = 1000
) {
  horizon <- length(daily_lambda)
  runs <- vector("list", n_sim)

  for (sim in seq_len(n_sim)) {
    respiratory_occupancy <- numeric(horizon)

    for (day in seq_len(horizon)) {
      admissions_today <- rpois(1, daily_lambda[day])
      if (admissions_today == 0) {
        next
      }

      los_days <- pmax(1, ceiling(rweibull(admissions_today, shape = los_shape, scale = los_scale)))

      for (stay in los_days) {
        end_day <- min(horizon, day + stay - 1)
        respiratory_occupancy[day:end_day] <- respiratory_occupancy[day:end_day] + 1
      }
    }

    total_occupancy <- baseline_occupancy + respiratory_occupancy

    runs[[sim]] <- tibble(
      sim = sim,
      day = seq_len(horizon),
      occupancy = total_occupancy,
      overflow = pmax(total_occupancy - capacity, 0)
    )
  }

  bind_rows(runs)
}

# Escenarios
scenarios <- tribble(
  ~scenario,                          ~capacity, ~demand_multiplier, ~los_multiplier,
  "Base",                                  40,               1.00,            1.00,
  "Alta demanda",                          40,               1.20,            1.00,
  "Alta demanda + 6 camas",               46,               1.20,            1.00,
  "Alta demanda + estadia -10%",          40,               1.20,            0.90
) |>
  mutate(
    weekly_admissions = map(
      demand_multiplier,
      ~ demanda_base$ingresos_base * .x
    ),
    daily_lambda = map(weekly_admissions, daily_lambda_from_weekly),
    sim_data = pmap(
      list(daily_lambda, capacity, los_multiplier),
      \(daily_lambda, capacity, los_multiplier) {
        simulate_occupancy(
          daily_lambda = daily_lambda,
          capacity = capacity,
          los_shape = weibull_shape,
          los_scale = weibull_scale * los_multiplier,
          baseline_occupancy = base_occupancy,
          n_sim = n_sim
        )
      }
    )
  )

# Resultados y tablas
summary_table <- scenarios |>
  transmute(
    Escenario = scenario,
    Capacidad = capacity,
    Datos = sim_data
  ) |>
  rowwise() |>
  mutate(
    `Ocupación promedio` = mean(Datos$occupancy),
    `Pico promedio` = mean(tapply(Datos$occupancy, Datos$sim, max)),
    `Pico p90` = quantile(tapply(Datos$occupancy, Datos$sim, max), 0.90),
    `Prob. saturación` = mean(tapply(Datos$occupancy > Capacidad, Datos$sim, any)),
    `Exceso esperado cama-día` = mean(tapply(Datos$overflow, Datos$sim, sum))
  ) |>
  ungroup() |>
  mutate(
    across(where(is.numeric), ~ round(., 2))
  ) |>
  select(-Datos)

kable(summary_table, caption = "Resumen gerencial de escenarios de ocupación")

plot_data <- scenarios |>
  select(scenario, capacity, sim_data) |>
  unnest(sim_data) |>
  group_by(scenario, capacity, day) |>
  summarise(
    mean_occupancy = mean(occupancy),
    p90_occupancy = quantile(occupancy, 0.90),
    .groups = "drop"
  )

ggplot(plot_data, aes(day, mean_occupancy, color = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_linerange(aes(ymin = mean_occupancy, ymax = p90_occupancy), alpha = 0.15) +
  facet_wrap(~ scenario, ncol = 2) +
  geom_hline(aes(yintercept = capacity), linetype = 2, color = "firebrick") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Ocupación promedio y percentil 90 por escenario",
    subtitle = "Línea punteada: capacidad disponible",
    x = "Día del horizonte de planificación",
    y = "Camas ocupadas"
  ) +
  theme_minimal()

weekly_inputs <- demanda_base |>
  transmute(
    `Semana estadística` = SemanaEstadistica,
    `Urgencias respiratorias promedio` = round(urgencias_promedio, 1),
    `Ingresos esperados base` = round(ingresos_base, 1),
    `Ingresos esperados alta demanda` = round(ingresos_alta_demanda, 1)
  )

kable(weekly_inputs, caption = "Insumos semanales del caso")
