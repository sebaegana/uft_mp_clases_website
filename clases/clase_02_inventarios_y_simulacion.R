# Prerrequisitos
# - Paquetes: dplyr, ggplot2, tidyr, tibble.
# - No requiere datasets externos; el caso utiliza parametros definidos dentro del script.

# Paquetes
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)

# Configuracion del caso
params <- list(
  lambda = 15,  # demanda diaria
  days = 300,
  L = 4,
  K = 120,
  h = 8,
  CSL = 0.95
)

params$D <- params$lambda * params$days
params$Q_star <- sqrt(2 * params$K * params$D / params$h)
params$Q <- round(params$Q_star)
params$R <- qpois(params$CSL, lambda = params$lambda * params$L)
params$C_annual <- params$K * params$D / params$Q_star + params$h * params$Q_star / 2

# Resultado base
list(
  Q_star = params$Q_star,
  R = params$R,
  C_annual = params$C_annual
)

# Simulacion
simulate_inventory_qr <- function(lambda, days, L, Q, R, seed = 123) {
  set.seed(seed)

  inv <- Q
  on_order <- 0
  lt_queue <- integer(0)
  orders <- 0

  sim_rows <- vector("list", days)

  for (t in seq_len(days)) {
    arrival_flag <- 0

    if (length(lt_queue) > 0 && lt_queue[1] == 0) {
      inv <- inv + on_order
      on_order <- 0
      lt_queue <- lt_queue[-1]
      arrival_flag <- 1
    }

    if (length(lt_queue) > 0) {
      lt_queue <- lt_queue - 1
    }

    d <- rpois(1, lambda)
    stockout_today <- max(d - inv, 0)
    inv <- max(inv - d, 0)

    order_flag <- 0
    if (inv <= R && on_order == 0) {
      on_order <- Q
      lt_queue <- c(lt_queue, L)
      orders <- orders + 1
      order_flag <- 1
    }

    sim_rows[[t]] <- tibble(
      day = t,
      demand = d,
      inv_onhand = inv,
      inv_pos = inv + on_order,
      order_placed = order_flag,
      arrival = arrival_flag,
      stockout_units = stockout_today
    )
  }

  sim_df <- bind_rows(sim_rows) %>%
    mutate(cum_stockouts = cumsum(stockout_units))

  resumen <- list(
    Q_star = Q,
    R = R,
    orders = orders,
    end_inventory = sim_df$inv_onhand[nrow(sim_df)],
    total_stockouts = sum(sim_df$stockout_units)
  )

  list(data = sim_df, summary = resumen)
}

# Simulacion simple de 1 año
sim_simple <- simulate_inventory_qr(
  lambda = params$lambda,
  days = params$days,
  L = params$L,
  Q = params$Q,
  R = params$R
)

list(
  orders = sim_simple$summary$orders,
  stockouts = sim_simple$summary$total_stockouts,
  end_inventory = sim_simple$summary$end_inventory
)

# Graficos
sim <- simulate_inventory_qr(
  lambda = params$lambda,
  days = params$days,
  L = params$L,
  Q = params$Q,
  R = params$R
)

df <- sim$data
resumen <- sim$summary
resumen

ggplot(df, aes(day, inv_onhand)) +
  geom_line() +
  geom_hline(yintercept = params$R, linetype = 2) +
  labs(
    title = "Trayectoria del inventario (on-hand)",
    x = "Día",
    y = "Unidades"
  ) +
  annotate("text", x = max(df$day) * 0.85, y = params$R + 5, label = paste0("R = ", params$R))

ggplot(df, aes(day, inv_onhand)) +
  geom_line() +
  geom_point(
    data = df %>% filter(order_placed == 1),
    aes(day, inv_onhand),
    shape = 24,
    size = 2
  ) +
  geom_point(
    data = df %>% filter(arrival == 1),
    aes(day, inv_onhand),
    shape = 21,
    size = 2
  ) +
  geom_hline(yintercept = params$R, linetype = 2) +
  labs(
    title = "Inventario con eventos (pedidos y arribos)",
    x = "Día",
    y = "Unidades",
    caption = "▲ = pedido emitido | ○ = pedido recibido"
  )

ggplot(df, aes(day, cum_stockouts)) +
  geom_line() +
  labs(
    title = "Demanda insatisfecha acumulada (stockouts)",
    x = "Día",
    y = "Unidades acumuladas"
  )

ggplot(df, aes(demand)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left") +
  labs(
    title = "Distribución de la demanda diaria (Poisson)",
    x = "Unidades por día",
    y = "Frecuencia"
  )
