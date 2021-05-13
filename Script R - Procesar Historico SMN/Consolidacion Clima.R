
# ==================================================
# Script      'Consolidacion Clima.R'
# Propósito   Procesar los datos del clima obtenidios desde el SMN, sección 'Datos Abiertos'.
#             Ubicar este script dentro del directorio con los archivos de input.
# URL         https://www.smn.gob.ar/descarga-de-datos
#             https://ssl.smn.gob.ar/dpd/descarga_opendata.php?file=observaciones/datohorario20191101.txt
#
# Input       Archivos 'datohorarioYYYYMMDD.txt' del SMN.
# Output      Archivo 'mediciones del clima.csv' con los datos del clima consolidados.
# ==================================================


# install.packages("dplyr")   # instalar la librería "dplyr"
library("dplyr")              # cargar la librería "dplyr"

# aumentar el default para que se muestren todos los datos del dataframe
options(max.print=15000)

# setear el working directory a la ubicación actual del script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# ==================================================
# Construir un rango de fechas en formato texto.
# ==================================================
buildDateRange <- function(daysBack = 2) {
  dates <- format(seq(from = as.Date("2019-08-01"), to = Sys.Date()-daysBack, by = "day"), "%Y%m%d")
}


# ==================================================
# Construir nombres de archivos para el rango de fechas recibido.
# ==================================================
buildFileNames <- function(dateRange) {
  basepath = paste(getwd(), '/', sep='')    # ruta dinámina al documento actual
  basename = 'datohorario'
  baseext = '.txt'

  fileNames <- paste(basepath, basename, dateRange, baseext, sep='')
}


# ==================================================
# Leer y combinar el contenido de los nombre de archivos recibidos.
# Se filtra el resultado a los datos de la estación meteorológica Aeroparque.
# ==================================================
getFilesContent <- function(fileNames) {
  filesContent <- NULL
  colWidths <- c(8, 6, 6, 5, 8, 5, 5, 57)
  colNames <- c('FechaOrig', 'Hora', 'Temp', 'Humedad', 'Presion', 'DD', 'FF', 'Estacion')
  colClass <- c('character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'character')
  
  for (filename in fileNames)
  {
    tryCatch (
    {
      fileContent <- read.fwf(file=filename, skip=2, widths=colWidths, col.names=colNames, colClasses=colClass)
      filesContent <- rbind(filesContent, fileContent) # el merge() no funciona
      # print(fileContent)
    },
    error = function(e) { message(paste('- error:', e)) },
    warning = function(w) { message(paste('- warning:', w)) }
    )
  }

  filesContent
}


# ==================================================
# Eliminar los datos irrelevantes.
# ==================================================
removeUselessData <- function(filesContent) {
  filterString <- "     AEROPARQUE AERO                                     "
  
  filteredContent <- filter(filesContent, Estacion == filterString)
  filteredContent <- mutate(filteredContent, Fecha = as.Date(FechaOrig, "%d%m%Y"))
  filteredContent <- select(filteredContent, -FechaOrig, -DD, -FF, -Estacion)
  filteredContent <- select(filteredContent, Fecha, everything())
  # filteredContent$DD <- NULL
  # print(filteredContent)
  
  filteredContent
}


# ==================================================
# Construir la estructura para los datos del clima
# Noche 0-7hs, Mañana 8-15hs, Tarde 16-23hs
# ==================================================
buildWeatherData <- function(filteredContent) {
  morning <- c(8:15)
  evening <- c(16:23)
  night <- c(0:7)
  
  dfNight <- filteredContent %>%
    filter(Hora %in% night) %>%
    group_by(Fecha) %>%
    summarize(NocheTemp = mean(Temp), NocheHumedad = mean(Humedad), NochePresion = mean(Presion))

  dfMorning <- filteredContent %>%
    filter(Hora %in% morning) %>%
    group_by(Fecha) %>%
    summarize(MañanaTemp = mean(Temp), MañanaHumedad = mean(Humedad), MañanaPresion = mean(Presion))

  dfEvening <- filteredContent %>%
    filter(Hora %in% evening) %>%
    group_by(Fecha) %>%
    summarize(TardeTemp = mean(Temp), TardeHumedad = mean(Humedad), TardePresion = mean(Presion))

  weatherData <- data.frame(dfNight, dfMorning, dfEvening)
  weatherData <- select(weatherData, -Fecha.1, -Fecha.2)
  weatherData <- arrange(weatherData, Fecha)
  
  weatherData
}


# ==================================================
# Grabar un archivo con los datos del clima
# ==================================================
saveWeatherData <- function(weatherData) {
  
  filename = 'mediciones del clima.csv'
  fullname = paste(getwd(), '/', filename, sep='')    # ruta dinámina al documento actual
  
  write.csv(weatherData, file=fullname)
}


# ==================================================
# MAIN
# ==================================================
print("Step #01: buildDateRange() ...")
dateRange <- buildDateRange(1)

print("Step #02: buildFileNames() ...")
fileNames <- buildFileNames(dateRange)

print("Step #03: getFilesContent() ...")
filesContent <- getFilesContent(fileNames)

print("Step #04: removeUselessData() ...")
filteredContent <- removeUselessData(filesContent)

print("Step #05: buildWeatherData() ...")
weatherData <- buildWeatherData(filteredContent)

print("Step #06: saveWeatherData() ...")
saveWeatherData(weatherData)

print('DONE!')
