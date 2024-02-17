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

#Estadísticas descriptivas
#Rango Intercuartilico(IQR) para hayar valores atipicos
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
# boxplot 
ggplot(data_filtered, aes(y = charges)) +
  geom_boxplot(fill = "lightblue", colour = "darkblue") +
  geom_jitter(color = "red", width = 0.2) +
  theme_minimal() +
  labs(title = "Boxplot de Charges", y = "Charges", x = "")

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

#Boxplot de Charge
ggplot(DF, aes(y = charges)) +
  geom_boxplot(fill = "lightblue", colour = "darkblue") +
  geom_jitter(aes(color = ifelse(outliersCharges, "red", "black")), width = 0.2) +
  theme_minimal() +
  labs(title = "Boxplot de Charges con Valores Atípicos", y = "Charges", x = "") +
  scale_color_manual(values = c("red" = "red", "black" = "black"))



# Filtrar los valores atípicos
#outliers <- subset(data, data$edad < lower | data$edad > upper)

# esto solo esestaditcia descrptiva
DF %>%
  group_by(Seccional) %>%
  dplyr::summarise(Prom    = mean(Salario, na.rm = TRUE),
                   Mediana = median(Salario, na.rm = TRUE),
                   Moda = mfv(Salario, na_rm = FALSE),
                   DE      = sd(Salario, na.rm = TRUE),
                   Max     = max(Salario, na.rm = TRUE),
                   Min     = min(Salario, na.rm = TRUE),
                   CV       = sd(Salario, na.rm = TRUE)/mean(Salario, na.rm = TRUE)*100)