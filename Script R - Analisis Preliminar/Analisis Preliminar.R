# cargar la librer�a "dplyr"
library("dplyr")

# setear el working directory a la ubicaci�n actual del script
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

# normalizaci�n
datos_n <- data.frame('Fecha' = datos$Fecha, 'pH' = datos$pH, 'Delta' = scale(datos$Delta), 'Temp' = scale(datos$Temp), 'Humedad' = scale(datos$Humedad), 'Presion' = scale(datos$Presion))
hist(datos_n$Delta)

# check y filtro de valores at�picos
boxplot(datos_n)
datos_n <- filter(datos_n, Delta > -1.7 & Delta < 1.7) # eliminar valores at�picos
boxplot(datos_n)

# inspecci�n visual de correlaciones
pairs(datos_n)

# an�lisis de correlaciones
cor.test(x = datos_n$Temp, y = datos_n$Delta, alternative = "two.sided", conf.level = 0.95, method = "pearson")
cor.test(x = datos_n$Humedad, y = datos_n$Delta, alternative = "two.sided", conf.level = 0.95, method = "pearson")
cor.test(x = datos_n$Presion, y = datos_n$Delta, alternative = "two.sided", conf.level = 0.95, method = "pearson")
# NINGUNA CORRELACION << ---- !!!!!!

# regresi�n lineal
modelo <- lm(Delta~Temp, datos_n)
summary(modelo)

plot(datos_n$Temp, datos_n$Delta)
abline(modelo)
