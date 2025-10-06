# Paquetes
library(readxl)
library(dplyr)
library(knitr)
library(fitdistrplus)
library(survival)
library(flexsurv)
library(tidyr)
library(purrr)

# 1) URL RAW del .xlsm y hoja 'viarapida'
raw_url <- "https://raw.githubusercontent.com/Raydonal/ML-Weibull/main/viarapida.xlsm"  # cambia si es otro repo
tmp <- tempfile(fileext = ".xlsm")
download.file(raw_url, tmp, mode = "wb", quiet = TRUE)
df <- read_excel(tmp, sheet = "viarapida", .name_repair = "unique") |>
  transmute(TCC = as.numeric(TCC), DELTA = as.integer(DELTA)) |>
  filter(is.finite(TCC), TCC > 0, DELTA %in% c(0,1))

stopifnot(nrow(df) > 0)

# --- Conteos clave ---
n_total    <- nrow(df)
n_event    <- sum(df$DELTA == 1)
n_censored <- sum(df$DELTA == 0)
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

# --- Resumen estadístico (global y por estado) ---
quant_levels <- c(0, .1, .25, .5, .75, .9, 1)
qnames <- paste0("q", quant_levels*100)

resumen_global <- df %>%
  summarise(
    n     = n(),
    min   = min(TCC),
    mean  = mean(TCC),
    sd    = sd(TCC),
    median= median(TCC),
    max   = max(TCC),
  )


resumen_por_estado <- df %>%
  group_by(DELTA) %>%
  summarise(
    n     = n(),
    min   = min(TCC),
    mean  = mean(TCC),
    sd    = sd(TCC),
    median= median(TCC),
    max   = max(TCC),
    .groups = "drop"
  )

kable(resumen_global, digits = 3,
      caption = paste("Resumen global de TCC (", units_label, ")", sep=""))

kable(resumen_por_estado, digits = 3,
      caption = paste("Resumen de TCC por estado DELTA (", units_label, ")", sep=""))

# --- Histogramas + densidad ---
ggplot(df, aes(TCC)) +
  geom_histogram(bins = 30, aes(y = after_stat(density))) +
  geom_density(linewidth = 1) +
  labs(title = "Histograma + Densidad de TCC (global)",
       x = paste("TCC (", units_label, ")", sep=""),
       y = "Densidad") +
  theme_minimal()

ggplot(df, aes(TCC, fill = factor(DELTA))) +
  geom_histogram(bins = 30, position = "identity", alpha = .4) +
  labs(title = "Histograma por estado (DELTA: 1=muerte, 0=censura)",
       x = paste("TCC (", units_label, ")", sep=""),
       fill = "DELTA") +
  theme_minimal()

# --- Boxplot por estado ---
ggplot(df, aes(x = factor(DELTA), y = TCC)) +
  geom_boxplot(outlier.alpha = .5) +
  labs(title = "Boxplot de TCC por estado",
       x = "DELTA (1=muerte, 0=censura)",
       y = paste("TCC (", units_label, ")", sep="")) +
  theme_minimal()

# --- ECDF (distribución empírica acumulada) ---
ggplot(df, aes(TCC)) +
  stat_ecdf(geom = "step") +
  labs(title = "ECDF de TCC (global)",
       x = paste("TCC (", units_label, ")", sep=""),
       y = "F_hat(t)") +
  theme_minimal()

# 2) Ajustes Weibull
#   a) SIN censura: solo eventos
x_evt <- df$TCC[df$DELTA == 1]
fd_nc <- fitdistrplus::fitdist(x_evt, "weibull")
k_nc   <- unname(fd_nc$estimate["shape"])
lam_nc <- unname(fd_nc$estimate["scale"])

#   b) CON censura: todos
fit_c  <- flexsurv::flexsurvreg(Surv(TCC, DELTA) ~ 1, data = df, dist = "weibull")
k_c    <- fit_c$res["shape","est"]
lam_c  <- fit_c$res["scale","est"]

params_tbl <- tibble(
  Caso = c("Sin censura (DELTA==1)", "Con censura (Surv)"),
  `shape (k)` = c(k_nc, k_c),
  `scale (λ)` = c(lam_nc, lam_c)
)
kable(params_tbl, digits = 4, caption = "Parámetros Weibull 2P por caso")

# 3) Funciones y resúmenes
w_pdf <- function(t,k,lam) (k/lam)*(t/lam)^(k-1)*exp(-(t/lam)^k)
w_cdf <- function(t,k,lam) 1 - exp(-(t/lam)^k)
w_sur <- function(t,k,lam) exp(-(t/lam)^k)
w_haz <- function(t,k,lam) (k/lam)*(t/lam)^(k-1)
w_H   <- function(t,k,lam) (t/lam)^k
w_tp  <- function(p,k,lam) lam * (-log(1-p))^(1/k)
w_med <- function(k,lam)   lam * (log(2))^(1/k)
w_mean<- function(k,lam)   lam * gamma(1 + 1/k)

# puntos de evaluación (quintiles del rango observado)
t_eval <- quantile(df$TCC, probs = c(.1,.3,.5,.7,.9), na.rm = TRUE) |> as.numeric()
t_cols <- paste0("t=", round(t_eval, 2))

calc_block <- function(k, lam, caso, t_eval, t_cols) {
  vals <- list(
    `PDF f(t)`            = w_pdf(t_eval, k, lam),
    `CDF F(t)`            = w_cdf(t_eval, k, lam),
    `Supervivencia S(t)`  = w_sur(t_eval, k, lam),
    `Riesgo h(t)`         = w_haz(t_eval, k, lam),
    `Riesgo acum. H(t)`   = w_H  (t_eval, k, lam)
  )
  mat <- do.call(rbind, lapply(vals, function(v) round(v, 6)))
  colnames(mat) <- t_cols
  tibble(Caso = caso, Funcion = names(vals)) |>
    bind_cols(as_tibble(mat))
}

tbl_fun_nc <- calc_block(k_nc, lam_nc, "Sin censura", t_eval, t_cols)
tbl_fun_c  <- calc_block(k_c,  lam_c,  "Con censura", t_eval, t_cols)

kable(bind_rows(tbl_fun_nc, tbl_fun_c),
      caption = "Funciones evaluadas en t = p10, p30, p50, p70, p90 del TCC")

# Resúmenes: mediana, media y percentiles p
p_vec <- c(0.1, 0.5, 0.9)
summ_block <- function(k, lam, caso) {
  tibble(
    Caso = caso,
    Mediana = w_med(k, lam),
    Media   = w_mean(k, lam)
  ) |>
    bind_cols(
      tibble(Percentil = paste0("p=", p_vec),
             t_p = w_tp(p_vec, k, lam)) |>
        pivot_wider(names_from = Percentil, values_from = t_p)
    )
}
summ_tbl <- bind_rows(
  summ_block(k_nc, lam_nc, "Sin censura"),
  summ_block(k_c,  lam_c,  "Con censura")
)
kable(summ_tbl |> mutate(across(where(is.numeric), ~ round(., 4))),
      caption = "Mediana, media y percentiles t_p (p=0.1, 0.5, 0.9)")





library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(survival)

# --- Config ---
raw_url     <- "https://raw.githubusercontent.com/Raydonal/ML-Weibull/main/viarapida.xlsm"  # <--- reemplaza si corresponde
sheet_name  <- "viarapida"
units_label <- "horas"   # cambia si es "días", "meses", etc.

# --- Leer .xlsm (en binario) y hoja ---
tmp <- tempfile(fileext = ".xlsm")
download.file(raw_url, tmp, mode = "wb", quiet = TRUE)






