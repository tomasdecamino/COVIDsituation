library(wavelets)
# carga los daots de https://geovision.uned.ac.cr/oges/
# noten la fecha en el directorio y nombre de archivo
data <- read.csv("C:\\Users\\fbena\\Documents\\Python Scripts\\R Scripts\\05_20_21_CSV_GENERAL.csv", sep=';')
# hospitalizaciones en la columna 32
hosp <-data[,32]
# Se igualan a 0 los coeficientes indefinidos de la serie
hosp[is.na(hosp)]<-0
# Se derivan los datos de hospitalización para extraer la tasa de cambio en hospitalizaciones

deriv <- diff(hosp)
#Se establece una longitud de 2^N para la serie (en este caso, 512)
deriv[440:512]<-0

# Transformada wavelet, Daubechis 18. Se puede utiliuzar d# donde # es un par de  2 a 18. 
w <- dwt(deriv, filter="d18", n.levels=8, boundary="periodic", fast=TRUE)

#Se igualan los detalles a 0 de las bandas 1 al 5. Para la reconstrucción no es necesario modificar los
# escalamientos
w@W$W1[]<-0
w@W$W2[]<-0
w@W$W3[]<-0
w@W$W4[]<-0
w@W$W5[]<-0

# Reconstrucción wavelet
wi <- idwt(w,fast=TRUE)

plot(wi[0:440],type = 'l', col="red",xlab="Días", ylab="Velocidad", main="Velocidades de Hospitalización")
