# Tomás de Camino Beck
# tomas.decamino@gmail.com
# detalles de la fórmula acá https://ccp.ucr.ac.cr/index.php/tasa-r-covid-19.html

# se utiliza el paquete smoother para aplicar
# filtro de gauss al resultado
library(smoother)

#función de ponderación
weighted<-function(a){
  return(0.12 * exp(-0.0665*(a - 2) + (0.0048/0.2236) * (1 - exp(0.2236 * a))))
}

#para cada tiempo hace sumatoria ponderada u días atrás
effective.R<-function(d,u){
  n <- length(d)
  r <- vector(length = n)
  for (t in u:n) {
    r[t] <- d[t]/sum(rev(mapply(weighted, 2:(u+1) )* d[(t-u+1):t]))
  }
  return(r)
}


# carga los datos de https://geovision.uned.ac.cr/oges/
# noten la fecha en el directorio y nombre de archivo
data <- read.csv("https://geovision.uned.ac.cr/oges/archivos_covid/2021_05_24/05_24_21_CSV_GENERAL.csv", sep=';')
# casos nuevos positivos en la columna 3
hosp <-data[,3]

#calcula el R efectivo
R <- effective.R(hosp,15)

# grafica la derivada usando un filtro de gauss de 7 días
plot(smth(R,window = 7,method = "gaussian"),type = 'l', col="red",xlab="Días", ylab="R (efectivo)", main="Tasa de Contagio")
