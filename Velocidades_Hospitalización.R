# Tomás de Camino Beck
# tomas.decamino@gmail.com

# se utiliza el paquete smoother para aplicar
# filtro de gauss al resultado
library(smoother)

# Función para al cálculo de diferencias finitas
finite.differences <- function(data) {
  n <- length(data)
  fdx <- vector(length = n)
  for (i in 2:n) {
    fdx[i] <- (data[i] - data[i-1]) 
  }
  return(fdx)
}

# carga los daots de https://geovision.uned.ac.cr/oges/
# noten la fecha en el directorio y nombre de archivo
data <- read.csv("https://geovision.uned.ac.cr/oges/archivos_covid/2021_05_18/05_18_21_CSV_GENERAL.csv", sep=';')
# hospitalizaciones en la columna 30
hosp <-data[,30]
# calcula la derivada numérica
deriv <- finite.differences(hosp)
# grafica la derivada usando un filtro de gauss de 7 días
plot(smth(deriv,window = 7,method = "gaussian"),type = 'l', col="red",xlab="Días", ylab="Velocidad", main="Velocidades de Hospitalización")
