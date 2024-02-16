#########################################################################################
#Universidad Autónoma de Occidente
#HomeWork 1
########################################################################################

install.packages("readr")

#Cargar librerias
library(readr)

#Fijar el directorio de trabajo
setwd("P:/Estadistica y probabilidad 2/Homework1 febrero")

# Listar los archivos en el directorio de trabajo
dir()

#Cargar los datos
DF <- read_csv("dataset_HW1_insurance.csv")

# Visualizar las primera filas del conjunto de datos
head(DF)
