# cargar la librería "dplyr"
library("dplyr")

# setear el working directory a la ubicación actual del script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load & filtro de los datos
medicionesClima <- read.csv(file=paste(getwd(), '/mediciones del clima.csv', sep=''), header=TRUE)
medicionesClima <- select(medicionesClima, -X, -ManianaTemp, -ManianaHumedad, -ManianaPresion, -TardeTemp, -TardeHumedad, -TardePresion)
medicionesClima <- rename(medicionesClima, 'Temp' = 'NocheTemp', 'Humedad' = 'NocheHumedad', Presion = 'NochePresion')

medicionesPh <- read.csv(file=paste(getwd(), '/mediciones de pH.csv', sep=''), header=TRUE)
medicionesPh <- select(medicionesPh, -X)
medicionesPh <- rename(medicionesPh, 'pH' = 'A.Medir')

# merge de los datos
medicionesClima <- medicionesClima %>% filter(Fecha %in% medicionesPh$Fecha)
datos <- medicionesPh %>% left_join(medicionesClima, by = 'Fecha')

# normalización
datos_n <- data.frame('Fecha' = datos$Fecha, 'pH' = datos$pH, 'Delta' = scale(datos$Delta), 'Temp' = scale(datos$Temp), 'Humedad' = scale(datos$Humedad), 'Presion' = scale(datos$Presion))
hist(datos_n$Delta)

# check y filtro de valores atípicos
boxplot(datos_n)
datos_n <- filter(datos_n, Delta > -1.7 & Delta < 1.7) # eliminar valores atípicos
boxplot(datos_n)

# inspección visual de correlaciones
pairs(datos_n)

# análisis de correlaciones
cor.test(x = datos_n$Temp, y = datos_n$Delta, alternative = "two.sided", conf.level = 0.95, method = "pearson")
cor.test(x = datos_n$Humedad, y = datos_n$Delta, alternative = "two.sided", conf.level = 0.95, method = "pearson")
cor.test(x = datos_n$Presion, y = datos_n$Delta, alternative = "two.sided", conf.level = 0.95, method = "pearson")
# NINGUNA CORRELACION << ---- !!!!!!

# regresión lineal
modelo <- lm(Delta~Temp, datos_n)
summary(modelo)

plot(datos_n$Temp, datos_n$Delta)
abline(modelo)
