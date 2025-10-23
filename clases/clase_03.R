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
