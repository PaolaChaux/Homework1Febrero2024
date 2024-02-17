#########################################################################################
#Universidad Autónoma de Occidente
#HomeWork 1
########################################################################################

install.packages("readr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("modeest")
install.packages("hrbrthemes")


#Cargar librerias
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(modeest)
library(hrbrthemes)

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
#age
DF %>%
  dplyr::reframe(
    Prom    = mean(age, na.rm = TRUE),
    Mediana = median(age, na.rm = TRUE),
    Moda = mfv(age, na_rm = TRUE),
    DE      = sd(age, na.rm = TRUE),
    Max     = max(age, na.rm = TRUE),
    Min     = min(age, na.rm = TRUE),
    CV     = sd(age, na.rm = TRUE) / mean(age, na.rm = TRUE) * 100)


#BMI
DF %>%
  dplyr::reframe(Prom    = mean(bmi, na.rm = TRUE),
                   Mediana = median(bmi, na.rm = TRUE),
                   Moda = mfv(bmi, na_rm = TRUE),
                   DE      = sd(bmi, na.rm = TRUE),
                   Max     = max(bmi, na.rm = TRUE),
                   Min     = min(bmi, na.rm = TRUE),
                   CV       = sd(bmi, na.rm = TRUE)/mean(bmi, na.rm = TRUE)*100)

#Children(REVISAR, NO NETIENDO)
DF$children <- as.numeric(as.character(DF$children))

DF %>%
  dplyr::reframe(Prom    = mean(children, na.rm = TRUE),
                   Mediana = median(children, na.rm = TRUE),
                   Moda = mfv(children, na_rm = TRUE),
                   DE      = sd(children, na.rm = TRUE),
                   Max     = max(children, na.rm = TRUE),
                   Min     = min(children, na.rm = TRUE),
                   CV       = sd(children, na.rm = TRUE)/mean(children, na.rm = TRUE)*100)

#Charges
DF %>%
  dplyr::reframe(Prom    = mean(charges, na.rm = TRUE),
                   Mediana = median(charges, na.rm = TRUE),
                   Moda    = mfv(charges, na.rm = TRUE)[1],
                   DE      = sd(charges, na.rm = TRUE),
                   Max     = max(charges, na.rm = TRUE),
                   Min     = min(charges, na.rm = TRUE),
                   CV       = sd(charges, na.rm = TRUE)/mean(charges, na.rm = TRUE)*100)


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

#Asimetría y forma
install.packages("psych")
library(psych)

#Coeficiente de asimetría
skew(DF$age)
skew(DF$bmi)
skew(DF$children)
skew(DF$charges)

#Coeficiente de curtosis
kurtosi(DF$age)
kurtosi(DF$bmi)
kurtosi(DF$children)
kurtosi(DF$charges)
# necesitamos poner los cuartiles segun 2 variables?
#
#Cuartiles
#Datos_salario %>%
 # group_by(Facultad)%>%
  #dplyr::summarise(Q1 = as.numeric(quantile(Salario, 0.25)), 
   #                Q2 = as.numeric(quantile(Salario, 0.5)),
    #               Q3 = as.numeric(quantile(Salario, 0.75)))



#Correlacioness
#Diagrama de dispersion
#Charges y Age
ggplot(DF, aes(x = age, y = charges)) + 
  geom_point(
    color = "black",
    fill = "#69b3a2",
    shape = 22,
    alpha = 0.5,
    size = 1,
    stroke = 1
  ) +
  labs(title = "Relación entre Edad y Cargos Médicos", x = "Edad", y = "Cargos Médicos") +
  theme_ipsum()

#charges y bmi
ggplot(DF, aes(x = bmi, y = charges)) + 
  geom_point(
    color = "black",
    fill = "#69b3a2",
    shape = 22,
    alpha = 0.5,
    size = 1,
    stroke = 1
  ) +
  labs(title = "Relación entre BMI y Cargos Médicos", x = "BMI", y = "Cargos Médicos") +
  theme_ipsum()

#charges y children(este para mi no tiene ssentido o alguna importnacia porque son numeros enteros)
ggplot(DF, aes(x = children, y = charges)) + 
  geom_point(
    color = "black",
    fill = "#69b3a2",
    shape = 22,
    alpha = 0.5,
    size = 1,
    stroke = 1
  ) +
  labs(title = "Relación entre Número de Hijos y Cargos Médicos", x = "Número de Hijos", y = "Cargos Médicos") +
  theme_ipsum()

#Correlación muestra (r = 0.8397859)
numeric_df <- DF[, c("age", "bmi", "charges")]
cor(numeric_df, use = "complete.obs")

