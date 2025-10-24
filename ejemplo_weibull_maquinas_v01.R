library(tibble)
library(dplyr)
library(tidyr)
library(lubridate)

# 1️⃣ Dataframe ancho (igual a tu tabla)
failures <- tribble(
  ~Failure_no, ~M1_date,       ~M1_TBF, ~M2_date,       ~M2_TBF, ~M3_date,       ~M3_TBF,
  1, "29/01/2014", 1085,  "05/06/2015", 2450,  "29/05/2013", 413,
  2, "04/01/2016", 1904,  "29/05/2017", 2040.5, "12/07/2013", 101.5,
  3, "19/10/2016", 609,   "07/08/2017", 192.5,  "01/09/2014", 1130.5,
  4, "24/10/2016", 10.5,  "28/02/2018", 549.5,  "05/06/2015", 763,
  5, "29/05/2017", 605.5, "03/08/2018", 311.5,  "09/11/2015", 427,
  6, "06/03/2020", 2792,  "06/03/2020", 584.5,  "07/08/2017", 1750,
  7, "12/11/2020", 392,   "03/08/2020", 280,    "31/10/2018", 1228.5,
  8, NA,           NA,     NA,           NA,     "09/08/2019", 780.5,
  9, NA,           NA,     NA,           NA,     "06/03/2020", 560,
  10, NA,           NA,     NA,           NA,     "29/07/2020", 255.5,
  11, NA,           NA,     NA,           NA,     "07/01/2022", 1456
)

# --- pivot + parseo de fechas + selección (forzando dplyr/ tidyr) ---
failures_long <- failures |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("M"),
    names_to = c("machine", ".value"),
    names_pattern = "(M[0-9])_(date|TBF)"
  ) |>
  dplyr::mutate(
    Date_of_failure = lubridate::dmy(.data$date),
    TBF = as.numeric(.data$TBF)
  ) |>
  dplyr::select(machine, Failure_no, Date_of_failure, TBF) |>
  dplyr::arrange(machine, Failure_no)

library(fitdistrplus)

# Funciones Weibull
w_tp   <- function(p,k,lam) lam * (-log(1-p))^(1/k)
w_med  <- function(k,lam)   lam * (log(2))^(1/k)
w_mean <- function(k,lam)   lam * gamma(1 + 1/k)

# Ajuste por máquina
ajustes <- failures_long |>
  dplyr::filter(!is.na(.data$TBF)) |>
  dplyr::group_by(.data$machine) |>
  dplyr::summarise(fit = list(fitdistrplus::fitdist(.data$TBF, "weibull")),
                   .groups = "drop")

# Extraer parámetros y métricas
resumen <- ajustes |>
  dplyr::rowwise() |>
  dplyr::mutate(
    k   = unname(fit$estimate["shape"]),
    lam = unname(fit$estimate["scale"]),
    Mediana = w_med(k, lam),
    Media   = w_mean(k, lam),
    `p=0.1` = w_tp(0.1, k, lam),
    `p=0.5` = w_tp(0.5, k, lam),
    `p=0.9` = w_tp(0.9, k, lam)
  ) |>
  dplyr::ungroup() |>
  dplyr::transmute(                               # usamos transmute para evitar select()
    machine,
    `shape (k)` = k,
    `scale (η)` = lam,
    Mediana, Media, `p=0.1`, `p=0.5`, `p=0.9`
  ) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

knitr::kable(resumen, format = "markdown",
             caption = "Weibull 2P por componente (sin censura): parámetros y resúmenes")


library(fitdistrplus)
library(knitr)

# ---- Costos (solo importa el ratio) ----
C_PM <- 1
C_CM <- 20 * C_PM

# ---- Funciones Weibull y costo por hora (renovación exacta) ----
S <- function(t, k, eta) exp(-(t/eta)^k)

E_cycle_len <- function(T, k, eta) {
  # ∫_0^T S(t) dt (numérico)
  integrate(function(x) S(x, k, eta), lower = 0, upper = T,
            subdivisions = 2000, rel.tol = 1e-8)$value
}

cost_per_hour_renewal <- function(T, k, eta, C_PM, C_CM) {
  num <- C_PM * S(T, k, eta) + C_CM * (1 - S(T, k, eta))
  den <- E_cycle_len(T, k, eta)
  num / den
}

get_opt_T <- function(k, eta, C_PM, C_CM) {
  # Malla de búsqueda alrededor de la escala (ajusta si quieres)
  grid_T <- seq(0.2 * eta, 2.0 * eta, length.out = 200)
  costs  <- sapply(grid_T, cost_per_hour_renewal,
                   k = k, eta = eta, C_PM = C_PM, C_CM = C_CM)
  i_min <- which.min(costs)
  c(T_opt = grid_T[i_min], Costo_h_min = costs[i_min])
}

# ---- Estimar por máquina (base R) ----
machines <- unique(failures_long$machine)
rows <- lapply(machines, function(m) {
  x <- failures_long$TBF[failures_long$machine == m & !is.na(failures_long$TBF)]
  fit <- fitdistrplus::fitdist(x, "weibull")
  k   <- unname(fit$estimate["shape"])
  eta <- unname(fit$estimate["scale"])
  opt <- get_opt_T(k, eta, C_PM, C_CM)
  data.frame(
    machine = m,
    k = k,
    eta = eta,
    T_opt = opt["T_opt"],
    Costo_h_min = opt["Costo_h_min"],
    check.names = FALSE
  )
})

res <- do.call(rbind, rows)
is_num <- sapply(res, is.numeric)
res[ , is_num] <- lapply(res[ , is_num, drop = FALSE], function(z) round(z, 4))

knitr::kable(res, format = "markdown",
             caption = "Intervalo óptimo de mantenimiento preventivo (T*) por máquina con C_CM = 10×C_PM")