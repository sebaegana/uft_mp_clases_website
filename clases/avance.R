install.packages("arrow")   # once
library(arrow)
library(tidyverse)

# Read a single Parquet file
df <- read_parquet("data/at_urg_respiratorio_semanal.parquet")   # returns a data.frame/tibble

unique(df$ComunaGlosa)

sotero <- 
  df %>% 
  filter(RegionCodigo == 13 
         & ServicioSaludCodigo == 14 
         & ComunaCodigo == 13201
         & EstablecimientoGlosa == "Complejo Hospitalario Dr. Sótero del Río (Santiago, Puente Alto)") %>% 
  arrange(Anio, SemanaEstadistica)

sotero_01 <- 
  df %>% 
  filter(RegionCodigo == 13 
                       & ServicioSaludCodigo == 14 
                       & ComunaCodigo == 13201
                       & EstablecimientoGlosa == "Complejo Hospitalario Dr. Sótero del Río (Santiago, Puente Alto)") %>%
  select(EstablecimientoGlosa, Anio, NumTotal, Causa) %>% 
  filter(Causa == "TOTAL CAUSAS SISTEMA RESPIRATORIO") %>% 
  group_by(EstablecimientoGlosa, Anio) %>%  
  summarise(total_casos = sum(NumTotal))

graph_01 <- 
  ggplot(sotero_01) + 
  geom_line(aes(Anio, total_casos))

graph_01

ggplot(sotero_01, aes(Anio, total_casos)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", span = 0.6, se = FALSE) +         # tendencia suave
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +    # tendencia lineal
  labs(title = "TOTAL causas respiratorias – Sótero del Río",
       y = "Casos", x = "Año") +
  theme_minimal()

sotero_yoy <- sotero_01 %>%
  arrange(Anio) %>%
  mutate(yoy = 100 * (total_casos/lag(total_casos) - 1))

ggplot(sotero_yoy, aes(Anio, yoy)) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  labs(title = "Crecimiento interanual (%)", y = "%", x = "Año") +
  theme_minimal()

library(magick)
infile  <- "imagenes/Ejemplos-historicos-de-ciclos-economicos-1.webp"
outfile <- sub("\\.webp$", ".png", infile, ignore.case = TRUE)
image_write(image_read(infile), path = outfile, format = "png")
