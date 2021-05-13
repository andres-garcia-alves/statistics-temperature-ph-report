
# ==================================================
# Script      'Consolidacion pH.R'
# Propósito   Procesar las mediciones de pH según el template recibido.
#             Ubicar este script dentro del directorio con el archivo de input.
#
# Input       Archivo 'Mediciones.csv' exportado desde su archivo excel homónimo.
# Output      Archivo 'mediciones de ph.csv' con los datos del clima consolidados.
# ==================================================


# install.packages("dplyr")   # instalar la librería "dplyr"
library("dplyr")              # cargar la librería "dplyr"

# aumentar el default para que se muestren todos los datos del dataframe
options(max.print=15000)

# setear el working directory a la ubicación actual del script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# ==================================================
# Construir el nombre de archivo a leer.
# ==================================================
buildFileName <- function() {
  
  filename = 'Mediciones.csv'
  fullname = paste(getwd(), '/', filename, sep='')    # ruta dinámina al documento actual
}


# ==================================================
# Leer el contenido del nombre de archivo recibido.
# ==================================================
getFileContent <- function(fileName) {
  
  colNames <- c('A Medir', 'Calibracion 1', 'Calibracion 2', 'Real', 'Tiempo', 'Intentos', 'FechaOrig')
  colClass <- c('numeric', 'numeric', 'numeric', 'numeric', 'character', 'numeric', 'character')
  
  tryCatch (
    {
      fileContent <- read.csv(file=fileName, col.names=colNames, colClasses=colClass)
    },
    error = function(e) { message(paste('- error:', e)) },
    warning = function(w) { message(paste('- warning:', w)) }
  )

  fileContent
}


# ==================================================
# Eliminar los datos irrelevantes.
# ==================================================
removeUselessData <- function(fileContent) {
  
  filteredContent <- filter(fileContent, is.na(A.Medir) == FALSE)
  filteredContent <- filter(fileContent, A.Medir != 0)
  filteredContent <- mutate(filteredContent, Fecha = as.Date(FechaOrig, "%d/%m"))
  filteredContent <- select(filteredContent, -FechaOrig, -Tiempo, -Intentos, -Calibracion.1, -Calibracion.2)
  filteredContent <- mutate(filteredContent, Delta = A.Medir - Real)
  filteredContent <- select(filteredContent, Fecha, everything())

  filteredContent
}


# ==================================================
# Construir la estructura para los datos de pH
# ==================================================
buildPhData <- function(filteredContent) {

  phData <- filteredContent %>%
    group_by(Fecha, A.Medir) %>%
    summarize(Delta = mean(Delta))
  phData <- arrange(phData, Fecha)

  phData
}


# ==================================================
# Grabar un archivo con los datos de pH
# ==================================================
savePhData <- function(phData) {
  
  filename = 'mediciones de ph.csv'
  fullname = paste(getwd(), '/', filename, sep='')    # ruta dinámina al documento actual

  write.csv(phData, file=fullname)
}


# ==================================================
# MAIN
# ==================================================
print("Step #01: buildFileName() ...")
fileName <- buildFileName()

print("Step #02: getFileContent() ...")
fileContent <- getFileContent(fileName)

print("Step #03: removeUselessData() ...")
filteredContent <- removeUselessData(fileContent)

print("Step #04: buildPhData() ...")
phData <- buildPhData(filteredContent)

print("Step #05: savePhData() ...")
savePhData(phData)

print('DONE!')
