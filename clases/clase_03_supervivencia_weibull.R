# Paquetes
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(knitr)
library(fitdistrplus)
library(survival)
library(flexsurv)
library(lubridate)

# Configuracion del caso
raw_url <- "https://raw.githubusercontent.com/Raydonal/ML-Weibull/main/viarapida.xlsm"
sheet_name <- "viarapida"
units_label <- "horas"

# Carga y preparacion de datos
local_candidates <- c(
  file.path("clases", "data", "viarapida.xlsm"),
  file.path("data", "viarapida.xlsm")
)

local_path <- local_candidates[file.exists(local_candidates)][1]
tmp <- tempfile(fileext = ".xlsm")

download_ok <- tryCatch({
  download.file(raw_url, tmp, mode = "wb", quiet = TRUE)
  file.exists(tmp) && file.info(tmp)$size > 0
}, error = function(e) FALSE, warning = function(w) FALSE)

file_to_read <- if (isTRUE(download_ok)) {
  tmp
} else if (!is.na(local_path)) {
  local_path
} else {
  stop("No se pudo descargar 'viarapida.xlsm' y tampoco existe una copia local utilizable.")
}

df <- read_excel(file_to_read, sheet = sheet_name, .name_repair = "unique") |>
  transmute(
    TCC = suppressWarnings(as.numeric(TCC)),
    DELTA = suppressWarnings(as.integer(DELTA))
  ) |>
  filter(is.finite(TCC), TCC > 0, DELTA %in% c(0L, 1L))

stopifnot(nrow(df) > 0)

# Resultados descriptivos
n_total <- nrow(df)
n_event <- sum(df$DELTA == 1L)
n_censored <- sum(df$DELTA == 0L)
event_rate <- n_event / n_total

kable(
  data.frame(
    Total = n_total,
    Eventos_DELTA1 = n_event,
    Censuras_DELTA0 = n_censored,
    Proporcion_evento = round(event_rate, 3)
  ),
  caption = "Conteos: total, eventos (DELTA=1), censuras (DELTA=0)"
)

resumen_global <- df %>%
  summarise(
    n = n(),
    min = min(TCC),
    mean = mean(TCC),
    sd = sd(TCC),
    median = median(TCC),
    max = max(TCC)
  )

resumen_por_estado <- df %>%
  group_by(DELTA) %>%
  summarise(
    n = n(),
    min = min(TCC),
    mean = mean(TCC),
    sd = sd(TCC),
    median = median(TCC),
    max = max(TCC),
    .groups = "drop"
  )

kable(
  resumen_global,
  digits = 3,
  caption = paste("Resumen global de TCC (", units_label, ")", sep = "")
)

kable(
  resumen_por_estado,
  digits = 3,
  caption = paste("Resumen de TCC por estado DELTA (", units_label, ")", sep = "")
)

# Graficos descriptivos
ggplot(df, aes(TCC)) +
  geom_histogram(bins = 30, aes(y = after_stat(density))) +
  geom_density(linewidth = 1) +
  labs(
    title = "Histograma + Densidad de TCC (global)",
    x = paste("TCC (", units_label, ")", sep = ""),
    y = "Densidad"
  ) +
  theme_minimal()

ggplot(df, aes(TCC, fill = factor(DELTA))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.4) +
  labs(
    title = "Histograma por estado (DELTA: 1 = muerte, 0 = censura)",
    x = paste("TCC (", units_label, ")", sep = ""),
    fill = "DELTA"
  ) +
  theme_minimal()

ggplot(df, aes(x = factor(DELTA), y = TCC)) +
  geom_boxplot(outlier.alpha = 0.5) +
  labs(
    title = "Boxplot de TCC por estado",
    x = "DELTA (1 = muerte, 0 = censura)",
    y = paste("TCC (", units_label, ")", sep = "")
  ) +
  theme_minimal()

ggplot(df, aes(TCC)) +
  stat_ecdf(geom = "step") +
  labs(
    title = "ECDF de TCC (global)",
    x = paste("TCC (", units_label, ")", sep = ""),
    y = "F_hat(t)"
  ) +
  theme_minimal()

# Ajustes y resultados Weibull
x_evt <- df$TCC[df$DELTA == 1L]
fd_nc <- fitdist(x_evt, "weibull")
k_nc <- unname(fd_nc$estimate["shape"])
lam_nc <- unname(fd_nc$estimate["scale"])

fit_c <- flexsurvreg(Surv(TCC, DELTA) ~ 1, data = df, dist = "weibull")
k_c <- fit_c$res["shape", "est"]
lam_c <- fit_c$res["scale", "est"]

params_tbl <- tibble(
  Caso = c("Sin censura (DELTA==1)", "Con censura (Surv)"),
  `shape (k)` = c(k_nc, k_c),
  `scale (eta)` = c(lam_nc, lam_c)
)

kable(params_tbl, digits = 4, caption = "Parámetros Weibull 2P por caso")

w_pdf <- function(t, k, lam) (k / lam) * (t / lam)^(k - 1) * exp(-(t / lam)^k)
w_cdf <- function(t, k, lam) 1 - exp(-(t / lam)^k)
w_sur <- function(t, k, lam) exp(-(t / lam)^k)
w_haz <- function(t, k, lam) (k / lam) * (t / lam)^(k - 1)
w_H <- function(t, k, lam) (t / lam)^k
w_tp <- function(p, k, lam) lam * (-log(1 - p))^(1 / k)
w_med <- function(k, lam) lam * (log(2))^(1 / k)
w_mean <- function(k, lam) lam * gamma(1 + 1 / k)

t_eval <- quantile(df$TCC, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE) |> as.numeric()
t_cols <- paste0("t=", round(t_eval, 2))

calc_block <- function(k, lam, caso, t_eval, t_cols) {
  vals <- list(
    `PDF f(t)` = w_pdf(t_eval, k, lam),
    `CDF F(t)` = w_cdf(t_eval, k, lam),
    `Supervivencia S(t)` = w_sur(t_eval, k, lam),
    `Riesgo h(t)` = w_haz(t_eval, k, lam),
    `Riesgo acum. H(t)` = w_H(t_eval, k, lam)
  )
  mat <- do.call(rbind, lapply(vals, function(v) round(v, 6)))
  colnames(mat) <- t_cols
  tibble(Caso = caso, Funcion = names(vals)) |>
    bind_cols(as_tibble(mat))
}

tbl_fun_nc <- calc_block(k_nc, lam_nc, "Sin censura", t_eval, t_cols)
tbl_fun_c <- calc_block(k_c, lam_c, "Con censura", t_eval, t_cols)

kable(
  bind_rows(tbl_fun_nc, tbl_fun_c),
  caption = "Funciones evaluadas en t = p10, p30, p50, p70, p90 del TCC"
)

p_vec <- c(0.1, 0.5, 0.9)
summ_block <- function(k, lam, caso) {
  tibble(
    Caso = caso,
    Mediana = w_med(k, lam),
    Media = w_mean(k, lam)
  ) |>
    bind_cols(
      tibble(Percentil = paste0("p=", p_vec), t_p = w_tp(p_vec, k, lam)) |>
        pivot_wider(names_from = Percentil, values_from = t_p)
    )
}

summ_tbl <- bind_rows(
  summ_block(k_nc, lam_nc, "Sin censura"),
  summ_block(k_c, lam_c, "Con censura")
) |>
  mutate(across(where(is.numeric), ~ round(., 4)))

kable(summ_tbl, caption = "Mediana, media y percentiles t_p (p = 0.1, 0.5, 0.9)")

# Caso aplicado: maquinas de hemodialisis
failures <- tribble(
  ~Failure_no, ~M1_date,       ~M1_TBF, ~M2_date,       ~M2_TBF, ~M3_date,       ~M3_TBF,
  1,           "29/01/2014",   1085,    "05/06/2015",   2450,    "29/05/2013",   413,
  2,           "04/01/2016",   1904,    "29/05/2017",   2040.5,  "12/07/2013",   101.5,
  3,           "19/10/2016",   609,     "07/08/2017",   192.5,   "01/09/2014",   1130.5,
  4,           "24/10/2016",   10.5,    "28/02/2018",   549.5,   "05/06/2015",   763,
  5,           "29/05/2017",   605.5,   "03/08/2018",   311.5,   "09/11/2015",   427,
  6,           "06/03/2020",   2792,    "06/03/2020",   584.5,   "07/08/2017",   1750,
  7,           "12/11/2020",   392,     "03/08/2020",   280,     "31/10/2018",   1228.5,
  8,           NA,             NA,      NA,             NA,      "09/08/2019",   780.5,
  9,           NA,             NA,      NA,             NA,      "06/03/2020",   560,
  10,          NA,             NA,      NA,             NA,      "29/07/2020",   255.5,
  11,          NA,             NA,      NA,             NA,      "07/01/2022",   1456
)

failures_long <- failures |>
  pivot_longer(
    cols = starts_with("M"),
    names_to = c("machine", ".value"),
    names_pattern = "(M[0-9])_(date|TBF)"
  ) |>
  mutate(
    Date_of_failure = dmy(.data$date),
    TBF = as.numeric(.data$TBF)
  ) |>
  dplyr::select(machine, Failure_no, Date_of_failure, TBF) |>
  arrange(machine, Failure_no)

kable(failures_long, format = "markdown", caption = "Fallas de máquinas")

ajustes <- failures_long |>
  filter(!is.na(.data$TBF)) |>
  group_by(.data$machine) |>
  summarise(fit = list(fitdist(.data$TBF, "weibull")), .groups = "drop")

resumen_maquinas <- ajustes |>
  rowwise() |>
  mutate(
    k = unname(fit$estimate["shape"]),
    eta = unname(fit$estimate["scale"]),
    Mediana = w_med(k, eta),
    Media = w_mean(k, eta),
    `p=0.1` = w_tp(0.1, k, eta),
    `p=0.5` = w_tp(0.5, k, eta),
    `p=0.9` = w_tp(0.9, k, eta)
  ) |>
  ungroup() |>
  transmute(
    machine,
    `shape (k)` = k,
    `scale (eta)` = eta,
    Mediana,
    Media,
    `p=0.1`,
    `p=0.5`,
    `p=0.9`
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

kable(
  resumen_maquinas,
  format = "markdown",
  caption = "Weibull 2P por componente (sin censura): parámetros y resúmenes"
)

# Mantenimiento preventivo
C_PM <- 1
C_CM <- 20 * C_PM

S_weibull <- function(t, k, eta) exp(-(t / eta)^k)

E_cycle_len <- function(T, k, eta) {
  integrate(
    function(x) S_weibull(x, k, eta),
    lower = 0,
    upper = T,
    subdivisions = 2000,
    rel.tol = 1e-8
  )$value
}

cost_per_hour_renewal <- function(T, k, eta, C_PM, C_CM) {
  num <- C_PM * S_weibull(T, k, eta) + C_CM * (1 - S_weibull(T, k, eta))
  den <- E_cycle_len(T, k, eta)
  num / den
}

get_opt_T <- function(k, eta, C_PM, C_CM) {
  grid_T <- seq(0.2 * eta, 2.0 * eta, length.out = 200)
  costs <- sapply(
    grid_T,
    cost_per_hour_renewal,
    k = k,
    eta = eta,
    C_PM = C_PM,
    C_CM = C_CM
  )
  i_min <- which.min(costs)
  c(T_opt = grid_T[i_min], Costo_h_min = costs[i_min])
}

machines <- unique(failures_long$machine)
rows <- lapply(machines, function(m) {
  x <- failures_long$TBF[failures_long$machine == m & !is.na(failures_long$TBF)]
  fit <- fitdist(x, "weibull")
  k <- unname(fit$estimate["shape"])
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
res[, is_num] <- lapply(res[, is_num, drop = FALSE], function(z) round(z, 4))

kable(
  res,
  format = "markdown",
  caption = "Intervalo óptimo de mantenimiento preventivo (T*) por máquina con C_CM = 20×C_PM"
)
