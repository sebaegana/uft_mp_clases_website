##### Ejemplo básico

lambda <- 15
days <- 300
D <- lambda * days
L <- 4
K <- 120
h <- 8
CSL <- 0.95

Q_star <- sqrt(2*K*D/h)
R <- qpois(CSL, lambda*L)
C_annual <- K*D/Q_star + h*Q_star/2

list(Q_star=Q_star, R=R, C_annual=C_annual)


# Parámetros
lambda <- 15      # demanda diaria
days   <- 300
D      <- lambda * days
L      <- 4
K      <- 120
h      <- 8
CSL    <- 0.95

# 1) EOQ
Q_star <- sqrt(2*K*D/h)

# 2) Punto de pedido con Poisson
R <- qpois(CSL, lambda = lambda*L)

# 3) Costo anual con Q*
C_annual <- K*D/Q_star + h*Q_star/2

list(Q_star = Q_star, R = R, C_annual = C_annual)

###################

# ---- Simulación simple de 1 año (demanda diaria Poisson, política (Q*,R)) ----
set.seed(123)
Q <- round(Q_star)
inv <-  Q          # inventario inicial
on_order <- 0      # unidades en tránsito
lt_queue <- integer(0) # cola de llegadas: días restantes hasta arribo

stockouts <- 0
orders <- 0

for (t in 1:days) {
  # Arribos
  if (length(lt_queue) > 0 && lt_queue[1] == 0) {
    inv <- inv + on_order
    on_order <- 0
    lt_queue <- lt_queue[-1]
  }
  lt_queue <- lt_queue - 1
  
  # Demanda del día
  d <- rpois(1, lambda)
  if (d <= inv) {
    inv <- inv - d
  } else {
    # quiebre (sin backorder en esta versión)
    stockouts <- stockouts + (d - inv)
    inv <- 0
  }
  
  # Revisión continua: si inventario <= R y no hay pedido en tránsito, ordenar Q
  if (inv <= R && on_order == 0) {
    on_order <- Q
    lt_queue <- c(lt_queue, L) # llega en L días
    orders <- orders + 1
  }
}

list(orders = orders, stockouts = stockouts, end_inventory = inv)

##### Simulación para gráficos

library(dplyr)
library(ggplot2)
library(tidyr)

# Parámetros
lambda <- 15      # demanda diaria
days   <- 300
D      <- lambda * days
L      <- 4
K      <- 120
h      <- 8
CSL    <- 0.95

# 1) EOQ y R
Q_star <- sqrt(2*K*D/h)
Q <- round(Q_star)
R <- qpois(CSL, lambda = lambda*L)

# 2) Simulación diaria (revisión continua (Q,R))
set.seed(123)
inv <- Q                 # inventario on-hand (inicial: un pedido completo)
on_order <- 0            # unidades en tránsito (0 al inicio)
lt_queue <- integer(0)   # cola de días hasta arribo de cada pedido

df <- tibble(
  day        = integer(),
  demand     = integer(),
  inv_onhand = integer(),
  inv_pos    = integer(), # posición: on-hand + on-order
  order_placed = integer(),
  arrival      = integer(),
  stockout_units = integer()
)

orders <- 0
cum_stockouts <- 0

for (t in 1:days) {
  
  arrival_flag <- 0
  # Arribo si corresponde (primero comprobamos arribo de pedidos previos)
  if (length(lt_queue) > 0 && lt_queue[1] == 0) {
    inv <- inv + on_order
    on_order <- 0
    lt_queue <- lt_queue[-1]
    arrival_flag <- 1
  }
  # Luego, corremos el reloj de los pedidos en tránsito
  if (length(lt_queue) > 0) lt_queue <- lt_queue - 1
  
  # Demanda Poisson del día
  d <- rpois(1, lambda)
  stockout_today <- 0
  if (d <= inv) {
    inv <- inv - d
  } else {
    stockout_today <- d - inv
    inv <- 0
  }
  cum_stockouts <- cum_stockouts + stockout_today
  
  # Política (Q,R): si inv <= R y NO hay pedido en tránsito, emitir pedido Q
  order_flag <- 0
  if (inv <= R && on_order == 0) {
    on_order <- Q
    lt_queue <- c(lt_queue, L)  # llegará en L días
    orders <- orders + 1
    order_flag <- 1
  }
  
  inv_pos <- inv + on_order
  
  df <- bind_rows(df, tibble(
    day = t,
    demand = d,
    inv_onhand = inv,
    inv_pos = inv_pos,
    order_placed = order_flag,
    arrival = arrival_flag,
    stockout_units = stockout_today
  ))
}

df <- df %>% mutate(cum_stockouts = cumsum(stockout_units))
resumen <- list(
  Q_star = Q_star, R = R,
  orders = orders,
  end_inventory = df$inv_onhand[days],
  total_stockouts = sum(df$stockout_units)
)
resumen

ggplot(df, aes(day, inv_onhand)) +
  geom_line() +
  geom_hline(yintercept = R, linetype = 2) +
  labs(title = "Trayectoria del inventario (on-hand)",
       x = "Día", y = "Unidades") +
  annotate("text", x = max(df$day)*0.85, y = R + 5, label = paste0("R = ", R))

ggplot(df, aes(day, inv_onhand)) +
  geom_line() +
  geom_point(data = df %>% filter(order_placed == 1),
             aes(day, inv_onhand), shape = 24, size = 2) +  # triángulo: pedido
  geom_point(data = df %>% filter(arrival == 1),
             aes(day, inv_onhand), shape = 21, size = 2) +  # círculo: arribo
  geom_hline(yintercept = R, linetype = 2) +
  labs(title = "Inventario con eventos (pedidos y arribos)",
       x = "Día", y = "Unidades",
       caption = "▲ = pedido emitido | ○ = pedido recibido")


ggplot(df, aes(day, cum_stockouts)) +
  geom_line() +
  labs(title = "Demanda insatisfecha acumulada (stockouts)",
       x = "Día", y = "Unidades acumuladas")



ggplot(df, aes(demand)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left") +
  labs(title = "Distribución de la demanda diaria (Poisson)",
       x = "Unidades por día", y = "Frecuencia")

