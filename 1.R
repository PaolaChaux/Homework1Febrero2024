#########################################################################################
#Universidad Autónoma de Occidente
#HomeWork 1
########################################################################################

install.packages("readr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")

#Cargar librerias
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

#Fijar el directorio de trabajo
setwd("P:/Estadistica y probabilidad 2/Homework1 febrero")

# Listar los archivos en el directorio de trabajo
dir()

#Cargar los datos
DF <- read_csv("dataset_HW1_insurance.csv")

# Visualizar las primera filas del conjunto de datos
head(DF)

# Verificar datos faltantes
sum(is.na(DF))

# Visualizar los datos faltantes
gg_miss_var(DF)

# Verificar si hay cadenas vacías en todas las columnas del dataframe
sapply(DF, function(x) any(x == ""))
sapply(DF, function(x) any(x == "", na.rm = TRUE))


#Estadísticas descriptivas
#Rango Intercuartilico(IQR) para hayar valores atipicos
#Age
valAtipAge = DF %>%
          summarise(Q1Age = quantile(DF$age, probs = 0.25, na.rm = TRUE),
                    Q2Age = quantile(DF$age, probs = 0.5, na.rm = TRUE),
                    Q3Age = quantile(DF$age, probs = 0.75, na.rm = TRUE),
                    IQRAge = Q3Age - Q1Age,
                    LimInfAge = Q1Age - 1.5 * IQRAge,
                    LimSupAge = Q3Age + 1.5 * IQRAge,
                    # Identificar valores atípicos
                    outliersAge = DF[DF$age < LimInfAge | DF$age > LimSupAge, ])
print(valAtipAge)

#Bmi 
valAtipBmi = DF %>%
  summarise(Q1Bmi  = quantile(DF$bmi, probs = 0.25, na.rm = TRUE),
            Q2Bmi  = quantile(DF$bmi, probs = 0.5, na.rm = TRUE),
            Q3Bmi  = quantile(DF$bmi, probs = 0.75, na.rm = TRUE),
            IQRBmi  = Q3Bmi  - Q1Bmi,
            LimInfBmi  = Q1Bmi  - 1.5 * IQRBmi ,
            LimSupBmi  = Q3Bmi  + 1.5 * IQRBmi ,
            # Identificar valores atípicos
            outliersBmi  = DF[DF$bmi < LimInfBmi  | DF$bmi > LimSupBmi , ])
print(valAtipBmi )

#Children
valAtipCHI = DF %>%
  summarise(Q1CHI  = quantile(DF$children, probs = 0.25, na.rm = TRUE),
            Q2CHI  = quantile(DF$children, probs = 0.5, na.rm = TRUE),
            Q3CHI  = quantile(DF$children, probs = 0.75, na.rm = TRUE),
            IQRCHI  = Q3CHI  - Q1CHI,
            LimInfCHI  = Q1CHI  - 1.5 * IQRCHI ,
            LimSupCHI  = Q3CHI  + 1.5 * IQRCHI ,
            # Identificar valores atípicos
            outliersCHI  = DF[DF$children < LimInfCHI  | DF$children > LimSupCHI , ])
print(valAtipCHI )

#Charges
valAtipCHA = DF %>%
  summarise(Q1CHA  = quantile(DF$charges, probs = 0.25, na.rm = TRUE),
            Q2CHA  = quantile(DF$charges, probs = 0.5, na.rm = TRUE),
            Q3CHA  = quantile(DF$charges, probs = 0.75, na.rm = TRUE),
            IQRCHA  = Q3CHA  - Q1CHA,
            LimInfCHA  = Q1CHA  - 1.5 * IQRCHA ,
            LimSupCHA  = Q3CHA  + 1.5 * IQRCHA ,
            # Identificar valores atípicos
            outliersCHA  = DF[DF$charges < LimInfCHA  | DF$charges > LimSupCHA , ])
print(valAtipCHA )

#Creacion del boxplot con puntos individuales de datos 
#Transformacion del DataFrame a alargo (porque?)
DF_long <- DF %>%
  pivot_longer(
    cols = c(age, bmi, children, charges), 
    names_to = "variable", 
    values_to = "value" )

# Boxplot
ggplot(DF_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.shape = NA) +  # No mostrar los outliers aquí para no sobreponerlos
  geom_jitter(width = 0.2, aes(color = variable), alpha = 0.5) +  # Añadir puntos individuales
  labs(x = "Variables", y = "Valores") +  # Etiquetas de los ejes
  theme_minimal() 

# Valores atipicos de Charges
valAtipCharges = DF %>%
  summarise(Q1Charges = quantile(charges, probs = 0.25, na.rm = TRUE),
            Q2Charges = quantile(charges, probs = 0.5, na.rm = TRUE),
            Q3Charges = quantile(charges, probs = 0.75, na.rm = TRUE),
            IQRCharges = Q3Charges - Q1Charges,
            LimInfCharges = Q1Charges - 1.5 * IQRCharges,
            LimSupCharges = Q3Charges + 1.5 * IQRCharges,
  outliersCharges = list(DF[DF$charges < LimInfCharges | DF$charges > LimSupCharges, ]))

print(valAtipCharges)

#Graficos
# Histograma Age
ggplot(DF, aes(x = age)) + 
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Distribución de la Edad", x = "Edad", y = "Frecuencia")

# Histograma BMI
ggplot(DF, aes(x = bmi)) + 
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(title = "Distribución del BMI", x = "BMI", y = "Frecuencia")

# Histograma Charges
ggplot(DF, aes(x = charges)) + 
  geom_histogram(bins = 30, fill = "pink", color = "black") +
  labs(title = "Distribución de los Cargos Médicos", x = "Cargos Médicos", y = "Frecuencia")

# Gráfico de densidad Age
ggplot(DF, aes(x = age)) + 
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(title = "Densidad de la Edad", x = "Edad", y = "Densidad")

# Gráfico de densidad BMI
ggplot(DF, aes(x = bmi)) + 
  geom_density(fill = "lightgreen", alpha = 0.5) +
  labs(title = "Densidad del BMI", x = "BMI", y = "Densidad")

# Gráfico de densidad Charges
ggplot(DF, aes(x = charges)) + 
  geom_density(fill = "pink", alpha = 0.5) +
  labs(title = "Densidad de los Cargos Médicos", x = "Cargos Médicos", y = "Densidad")

#Histogramas y Densidad en un solo grafico
#age
ggplot(data = DF, aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.5) +
  geom_density(color = "blue", alpha = 0.7) +
  labs(title = "Distribución y Densidad de la Edad", x = "Edad", y = "Densidad")
#children
ggplot(data = DF, aes(x = children)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", alpha = 0.5) +
  geom_density(color = "darkgreen", alpha = 0.7) +
  labs(title = "Distribución y Densidad del Número de Hijos", x = "Número de Hijos", y = "Densidad")
#bmi
ggplot(data = DF, aes(x = bmi)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "pink", alpha = 0.5) +
  geom_density(color = "red", alpha = 0.7) +
  labs(title = "Distribución y Densidad del BMI", x = "BMI", y = "Densidad")
#charges
ggplot(data = DF, aes(x = charges)) +
  geom_histogram(aes(y = ..density..), binwidth = 500, fill = "purple", alpha = 0.5) +
  geom_density(alpha = 0.7)

#Gráfico de barras para la variable children (ES NECESARIO?) yo creo que es mejor barras  que histo/densidad
DF$children <- factor(DF$children, exclude = NULL)
ggplot(data = DF, aes(x = children, fill = children)) +
  geom_bar() +
  scale_fill_viridis_d(begin = 0.3, end = 0.9, direction = 1, na.value = "grey") +
  labs(title = "Distribución del Número de Hijos", x = "Número de Hijos", y = "Cantidad") +
  theme_minimal()

#CATEGORICAS - CUALITATIVAS
# Gráfico de barras para la variable sex
DF$sex <- factor(DF$sex, exclude = NULL)
ggplot(data = DF, aes(x = sex, fill = sex)) +
  geom_bar() +
  scale_fill_manual(values = c("female" = "pink", "male" = "lightblue", "NA" = "grey"), na.translate = TRUE) +
  labs(title = "Distribución del Sexo", x = "Sexo", y = "Cantidad") +
  theme_minimal()

# Gráfico de barras para la variable smoker
DF$smoker <- factor(DF$smoker, exclude = NULL)
ggplot(data = DF, aes(x = smoker, fill = smoker)) +
  geom_bar() +
  scale_fill_manual(values = c("yes" = "orange", "no" = "lightgreen", "NA" = "grey"), na.translate = TRUE) +
  labs(title = "Distribución de Fumadores", x = "Fumador", y = "Cantidad") +
  theme_minimal()

# Gráfico de barras para la variable región
DF$region <- factor(DF$region, exclude = NULL)
ggplot(data = DF, aes(x = region, fill = region)) +
  geom_bar() +
  scale_fill_manual(values = c("northeast" = "steelblue", "southeast" = "gold", "southwest" = "coral", "northwest" = "olivedrab", "NA" = "grey"), na.translate = TRUE) +
  labs(title = "Distribución por Región", x = "Región", y = "Cantidad") +
  theme_minimal()

# esto solo esestaditcia descrptiva
#DF %>%
#  group_by(Seccional) %>%
#  dplyr::summarise(Prom    = mean(Salario, na.rm = TRUE),
#                   Mediana = median(Salario, na.rm = TRUE),
#                   Moda = mfv(Salario, na_rm = FALSE),
#                   DE      = sd(Salario, na.rm = TRUE),
#                   Max     = max(Salario, na.rm = TRUE),
#                   Min     = min(Salario, na.rm = TRUE),
#CV       = sd(Salario, na.rm = TRUE)/mean(Salario, na.rm = TRUE)*100)

#