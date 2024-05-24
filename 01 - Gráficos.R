# Instalamos el paquete tidyverse para operar con ggplot2. Luego cargamos la
# librería del paquete.
install.packages("tidyverse")
install.packages("modeest")
install.packages("lattice")
install.packages("reshape2")

library(conflicted)
library(tidyverse)
library(modeest)
library(lattice)
library(reshape2)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Ahora fijamos la base de datos y renombramos las columnas de la misma. 
attach(datos)
colnames(datos) <- c("Tiempo de residencia","Integrantes","Habitaciones","Cant. de personas por dormitorio","Presión","Baño/Letrina",
										 "Material del techo","Hum.(Dormitorios)","Hum.(Cocina)","Hum.(Baño)","Hum.(Living)",
										 "Hum.(No hay problema)","Hum.(Otro)","Estruct.(Dormitorios)","Estruct.(Cocina)", "Estruct.(Baño)", "Estruct.(Living)",
										 "Estruct.(No hay problema)", "Estruct.(Otro)")
total <- 327
# Luego, creamos los gráficos para cada variable y para las comparaciones de
# los mismos. También calculamos las medidas de posición correspondientes.

#Gráfico y valores para los habitantes de la vivienda.
datos_integrantes <- table(Integrantes)

plot(datos_integrantes,
     type = "h",
     main = "CANTIDAD DE PERSONAS POR VIVIENDA",
     xlab = "Integrantes",
     ylab = "Total de viviendas",
     las = 1)

moda_integrantes <- mfv(Integrantes)
valores_integrantes <- summary(Integrantes, quantile.type=3)
variancia_integrantes <- var(Integrantes)
desvio_estandar_integrantes <- sd(Integrantes)
rango_integrantes <- max(Integrantes) - min(Integrantes)
rango_intercuartilico_integrantes <- IQR(Integrantes)

#Gráfico y valores para la cantidad de personas por dormitorio.
datos_pers_dormitorios <- table(`Cant. de personas por dormitorio`)

plot(datos_pers_dormitorios,
     type = "h",
     main = "CANTIDAD DE PERSONAS POR DORMITORIO",
     xlab = "Integrantes por dormitorio",
     ylab = "Número de dormitorios",
     ylim = c(0, 140),
     las = 1)

moda_pers_dormitorios <-mfv(`Cant. de personas por dormitorio`)
valores_pers_dormitorios <- summary(`Cant. de personas por dormitorio`, 
                                    quantile.type=3)
variancia_pers_dormitorios <- var(`Cant. de personas por dormitorio`)
desvio_estandar_pers_dormitorios <- sd(`Cant. de personas por dormitorio`)
rango_pers_dormitorios <- max(`Cant. de personas por dormitorio`) -
                          min(`Cant. de personas por dormitorio`)
rango_intercuartilico_pers_dormitorios <- IQR(`Cant. de personas por dormitorio`)

#Gráfico y valores para la cantidad de habitaciones como dormitorios
datos_dormitorios <- table(Habitaciones)

plot(datos_dormitorios,
     type = "h",
     main = "AMBIENTES UTILIZADOS COMO DORMITORIOS",
     xlab = "Número de dormitorios",
     ylab = "Número de viviendas",
     ylim = c(0,150),
     las = 1)
axis(2, at = seq(0, 150, by = 25), las = 1)

moda_dormitorios <- mfv(Habitaciones)
valores_dormitorios <- summary(Habitaciones, quantile.type=3)
variancia_dormitorios <- var(Habitaciones)
desvio_estandar_dormitorios <- sd(Habitaciones)
rango_dormitorios <- max(Habitaciones) - min(Habitaciones)
rango_intercuartilico_dormitorios = IQR(Habitaciones)

#Gráfico y valores para el tiempo de residencia.
datos_tiempo_residencia <- table(`Tiempo de residencia`)

boxplot(`Tiempo de residencia`,
        horizontal = FALSE,
        col = "steelblue",
        main = "TIEMPO DE RESIDENCIA",
        ylab = "Años de residencia",
        ylim = c(0, 60),
        cex.sub = 0.7,
        boxwex = 0.5)

moda_tiempo_residencia <- mfv(`Tiempo de residencia`)
valores_tiempo_residencia <- summary(`Tiempo de residencia`, quantile.type=3)
variancia_tiempo_residencia <- var(`Tiempo de residencia`)
desvio_estandar_tiempo_residencia <- sd(`Tiempo de residencia`)
rango_tiempo_residencia <- max(`Tiempo de residencia`) - min(`Tiempo de residencia`)
rango_intercuartilico_tiempo_residencia <- IQR(`Tiempo de residencia`)

#Gráfico y valores para la presión del agua
datos_presion <- table(Presión)

porcentaje_grupo <- datos %>%
  group_by(Presión) %>%
  count() %>%
  ungroup() %>%
  mutate(Porcentaje = `n`/sum(`n`)*100)

ggplot(data = porcentaje_grupo,
       aes(x = 1, y = Porcentaje, fill = Presión)) +
       geom_bar(stat = "Identity") +
       geom_text(aes (label = paste0(round(Porcentaje,1),"%")),
                 position = position_stack(vjust = 0.5)) +
       coord_polar(theta = "y") + 
       theme_void() +
       ggtitle("PRESIÓN DEL AGUA") +
       theme(plot.title = element_text(hjust = 0.5), plot.title.position = "plot") +
       scale_fill_manual(values = c("papayawhip", "darksalmon", "darkgoldenrod"))

moda_presion <- mfv(Presión)
proporcion_presion_buena <- as.numeric(datos_presion["Buena"]) / total
proporcion_presion_débil <- as.numeric(datos_presion["Débil"]) / total
proporcion_presion_muy_débil <- as.numeric(datos_presion["Muy débil"]) / total

#Gráfico y valores para los baños en la vivienda
datos_baños <- table(`Baño/Letrina`)
datos_baños_ordenados <- sort(datos_baños, decreasing = TRUE)

barplot(datos_baños_ordenados,
        border = "black",
        main = "¿LA VIVIENDA POSEE BAÑO/LETRINA?",
        xlab = "Cantidad de viviendas",
        horiz = TRUE,
        col = "khaki",
        xlim = c(0, 300))

moda_baños <- mfv(`Baño/Letrina`)
proporcion_no <- as.numeric(datos_baños["No"]) / total
proporcion_dentro <- as.numeric(datos_baños["Si, dentro de la vivienda"]) / total
proporcion_fuera <- as.numeric(datos_baños["Si, fuera de la vivienda"]) / total

#Gráfico y valores para el material del techo
datos_material_techo <- table(`Material del techo`)
datos_material_techo_ordenados <- sort(datos_material_techo, decreasing = TRUE)

barplot(datos_material_techo_ordenados,
        border = "black",
        main = "MATERIAL DEL TECHO DE LA VIVIENDA",
        xlab = "Cantidad de viviendas",
        horiz = TRUE,
        col = "moccasin",
        xlim = c(0, 225))
axis(1, at = seq(0, 550, by = 25))

moda_materiales <- mfv(`Material del techo`)
proporcion_lona <- as.numeric(datos_material_techo["Lona"]) / total
proporcion_caña_adobe <- as.numeric(datos_material_techo["Caña/adobe"]) / total
proporcion_chapa <- as.numeric(datos_material_techo["Chapa"]) / total
proporcion_losa <- as.numeric(datos_material_techo["Losa de viguetas"]) / total

#Gráfico y valores para problemas de derrumbes
datos_derrumbes_dormitorios <- table(`Estruct.(Dormitorios)`)
datos_derrumbes_cocina <- table(`Estruct.(Cocina)`)
datos_derrumbes_baño <- table(`Estruct.(Baño)`)
datos_derrumbes_living <- table(`Estruct.(Living)`)
datos_derrumbes_no <- table(`Estruct.(No hay problema)`)
datos_derrumbes_otro <- table(`Estruct.(Otro)`)
derrumbes <- sort(c(datos_derrumbes_baño, datos_derrumbes_cocina,
                    datos_derrumbes_dormitorios, datos_derrumbes_living,
                    datos_derrumbes_no, datos_derrumbes_otro),
                  decreasing = TRUE)
nombres_derr <- c("Sin problemas", "Dormitorios", "Cocina", "Otro", "Baño", "Living")

barplot(derrumbes,
        border = "black",
        main = "PROBLEMAS DE DERRUMBES",
        xlab = "Cantidad de viviendas",
        horiz = TRUE,
        col = "moccasin",
        xlim = c(0, 275),
        names.arg = nombres_derr)
axis(1, at = seq(0, 275, by = 25))

moda_derrumbes <- names(which.max(derrumbes))
proporcion_sin_problema <- as.numeric(derrumbes["No hay ningún problema de filtraciones/humedad"]) / total
proporcion_dormitorios <- as.numeric(derrumbes["Dormitorios"]) / total
proporcion_cocina <- as.numeric(derrumbes["Cocina"]) / total
proporcion_otro <- as.numeric(derrumbes["Otro"]) / total
proporcion_baño <- as.numeric(derrumbes["Baño"]) / total
proporcion_living <- as.numeric(derrumbes["Living"]) / total

#Gráfico y valores para problemas de humedad
datos_humedad_dormitorios <- table(`Hum.(Dormitorios)`)
datos_humedad_cocina <- table(`Hum.(Cocina)`)
datos_humedad_baño <- table(`Hum.(Baño)`)
datos_humedad_living <- table(`Hum.(Living)`)
datos_humedad_no <- table(`Hum.(No hay problema)`)
datos_humedad_otro <- table(`Hum.(Otro)`)
humedad <- sort(c(datos_humedad_baño, datos_humedad_cocina,
                  datos_humedad_dormitorios, datos_humedad_living,
                  datos_humedad_no, datos_humedad_otro),
                decreasing = TRUE)
nombres_hum <- c("Sin problemas", "Dormitorios", "Baño", "Cocina", "Living", "Otro")

barplot(humedad,
        border = "black",
        main = "PROBLEMAS DE HUMEDAD",
        xlab = "Cantidad de viviendas",
        horiz = TRUE,
        col = "moccasin",
        xlim = c(0, 175),
        names.arg = nombres_hum)
axis(1, at = seq(0, 175, by = 25))

moda_humedad <- names(which.max(humedad))
proporcion_hum_sin_problema <- as.numeric(humedad["No hay ningún problema de filtraciones/humedad"]) / total
proporcion_hum_dormitorios <- as.numeric(humedad["Dormitorios"]) / total
proporcion_hum_baño <- as.numeric(humedad["Baño"]) / total
proporcion_hum_cocina <- as.numeric(humedad["Cocina"]) / total
proporcion_hum_living <- as.numeric(humedad["Living"]) / total
proporcion_hum_otro <- as.numeric(humedad["Otro"]) / total

# Gráfico de relación entre la cantidad de dormitorios y personas en los mismos.
rel_1 <- ggplot(datos, aes(x=Habitaciones, y=`Cant. de personas por dormitorio`))
rel_1 <- rel_1 + geom_jitter()
rel_1 <- rel_1 + xlab("Habitaciones como dormitorio") +
                 ylab("Cantidad de personas por dormitorio") +
                 ggtitle("RELACIÓN ENTRE LOS AMBIENTES USADOS COMO DORMITORIOS
Y LA CANTIDAD DE PERSONAS EN LOS MISMOS")
rel_1 <- rel_1 + theme(plot.title = element_text(hjust = 0.5), plot.title.position = "plot")
rel_1 <- rel_1 + scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6)) +
                 scale_y_continuous(breaks = c(1:10))

# Cálculos para la relación entre la presión del agua y los baños de la vivienda
contingencia <- addmargins(table(`Baño/Letrina`, Presión))
tabla_contingencia <- cbind(contingencia)

#Gráfico para la relación entre material del techo y problemas de derrumbes
tabla_mat_baño <- table(`Material del techo`, `Estruct.(Baño)`)
tabla_mat_cocina <- table(`Material del techo`, `Estruct.(Cocina)`)
tabla_mat_dorm <- table(`Material del techo`, `Estruct.(Dormitorios)`)
tabla_mat_living <- table(`Material del techo`, `Estruct.(Living)`)
tabla_mat_otro <- table(`Material del techo`, `Estruct.(Otro)`)
tabla_mat_no <- table(`Material del techo`, `Estruct.(No hay problema)`)
tabla_recuento <- cbind(tabla_mat_baño, tabla_mat_cocina, tabla_mat_dorm,
                                    tabla_mat_living, tabla_mat_otro, tabla_mat_no)

datos_material_derrumbe <- matrix(c(0,0,0,0,0,2,
                                    9,16,12,7,8,85,
                                    0,0,0,0,1,0,
                                    9,10,22,1,11,165),
                                  nrow = 4, byrow = TRUE,
                                  dimnames = list(c("Caña/adobe", "Chapa", "Lona",
                                                    "Losa de viguetas"),
                                                  c("Baño", "Cocina", "Dormitorios",
                                                    "Living", "Otro", "Sin problemas")))
datos_data_frame <- melt(datos_material_derrumbe)
datos_data_frame <- datos_data_frame %>% rename(Material = Var1)

ggplot(datos_data_frame, aes(x = Var2, y = value, fill = Material)) +
  geom_bar(stat = "identity") +
  ggtitle("RELACIÓN ENTRE EL MATERIAL DEL TECHO DE LA VIVIENDA
          Y LOS PROBLEMAS DE DERRUMBES DE LA MISMA") +
  theme(plot.title = element_text(hjust = 0.5), plot.title.position = "plot") +
  ylab("Cantidad") +
  xlab("Ambiente") +
  theme_minimal()

# Gráfico de relación entre tiempo de residencia y problemas de humedad
par(mfrow = c(2,3))

graf_temp_baño <- boxplot(`Tiempo de residencia`~`Hum.(Baño)`,
        horizontal = FALSE,
        col = "paleturquoise",
        main = "RELACIÓN ENTRE EL TIEMPO DE RESIDENCIA
Y LA HUMEDAD EN LOS BAÑOS",
        ylab = "Años de residencia",
        xlab = "Humedad en los baños",
        ylim = c(0, 60),
        cex.sub = 0.7,
        boxwex = 0.5)

graf_temp_dorm <- boxplot(`Tiempo de residencia`~`Hum.(Dormitorios)`,
        horizontal = FALSE,
        col = "cadetblue",
        main = "RELACIÓN ENTRE EL TIEMPO DE RESIDENCIA
Y LA HUMEDAD EN LOS DORMITORIOS",
        ylab = "Años de residencia",
        xlab = "Humedad en los dormitorios",
        ylim = c(0, 60),
        cex.sub = 0.7,
        boxwex = 0.5)

graf_temp_coci <- boxplot(`Tiempo de residencia`~`Hum.(Cocina)`,
        horizontal = FALSE,
        col = "bisque",
        main = "RELACIÓN ENTRE EL TIEMPO DE RESIDENCIA
Y LA HUMEDAD EN LA COCINA",
        ylab = "Años de residencia",
        xlab = "Humedad en la cocina",
        ylim = c(0, 60),
        cex.sub = 0.7,
        boxwex = 0.5)

graf_temp_liv <- boxplot(`Tiempo de residencia`~`Hum.(Living)`,
        horizontal = FALSE,
        col = "wheat",
        main = "RELACIÓN ENTRE EL TIEMPO DE RESIDENCIA
Y LA HUMEDAD EN EL LIVING",
        ylab = "Años de residencia",
        xlab = "Humedad en el living",
        ylim = c(0, 60),
        cex.sub = 0.7,
        boxwex = 0.5)

graf_temp_otro <- boxplot(`Tiempo de residencia`~`Hum.(Otro)`,
        horizontal = FALSE,
        col = "papayawhip",
        main = "RELACIÓN ENTRE EL TIEMPO DE RESIDENCIA
Y LA HUMEDAD EN OTROS AMBIENTES",
        ylab = "Años de residencia",
        xlab = "Humedad en otros ambientes",
        ylim = c(0, 60),
        cex.sub = 0.7,
        boxwex = 0.5)

graf_temp_no <- boxplot(`Tiempo de residencia`~`Hum.(No hay problema)`,
        horizontal = FALSE,
        col = "antiquewhite",
        main = "RELACIÓN ENTRE EL TIEMPO DE RESIDENCIA
Y LA INEXISTENCIA DE HUMEDAD",
        ylab = "Años de residencia",
        xlab = "Inexistencia de humedad",
        ylim = c(0, 60),
        cex.sub = 0.7,
        boxwex = 0.5)

par(mfrow = c(1,1))