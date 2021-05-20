# Tomás de Camino Beck
# tomas.decamino@gmail.com

# se utiliza el paquete smoother para aplicar
# filtro de gauss al resultado
library(smoother)
library(dplyr)
library(data.table)

# cambiar la fecha acordemente para cargar los últimos datos
aaaa <- "2021"
mm <- "05"
dd <- "18"

# Función para al cálculo de diferencias finitas
finite.differences <- function(data) {
  n <- length(data)
  fdx <- vector(length = n)
  for (i in 2:n) {
    fdx[i] <- (data[i] - data[i-1]) 
  }
  return(fdx)
}

# carga los datos de https://geovision.uned.ac.cr/oges/
url.data <- paste0("https://geovision.uned.ac.cr/oges/archivos_covid/",aaaa,"_",mm,"_",dd,"/",mm,"_",dd,"_",
                  substr(aaaa,start=3,stop=4),"_CSV_GENERAL.csv")

data <- as.data.frame(fread(url.data))

# hospitalizaciones en la columna 30
hosp <-data[,30]

# calcula la derivada numérica
deriv <- finite.differences(hosp)

# grafica la derivada usando un filtro de gauss de 7 días
plot(smth(deriv,window = 7,method = "gaussian"),type = 'l', col="red",xlab="Días", ylab="Velocidad", main="Velocidades de Hospitalización")
