# Instalamos los paquetes para leer archivos desde Excel y posteriormente
# filtrar los datos obtenidos. Luego, importamos las librerías correspondientes.

install.packages("readxl")
install.packages("tidyverse")

library(readxl)
library(conflicted)
library(tidyverse)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Cargamos el archivo .xlsx a una variable.
crudo <- read_excel("Datos_LP.xlsx",
                    col_names = TRUE,
                    skip = 2)

# Finalmente, filtramos los datos para conservar los que debemos analizar.
filtrado1 <- crudo %>% filter(...2 == "CABA" |
                              ...2 == "Santa Fe" |
                              ...2 == "Córdoba")
datos <- filtrado1[,c(5:6,13:14,26,29,63,73:84)]
