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
install.packages("naniar")
install.packages(c("knitr", "kableExtra", "rmarkdown"))
install.packages("tibble")
install.packages("xtable")
install.packages("openxlsx")

#Cargar librerias
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(modeest)
library(hrbrthemes)
library(naniar)
library(knitr)
library(kableExtra)
library(tibble)
library(openxlsx)

#Fijar el directorio de trabajo
setwd("P:/Estadistica y probabilidad 2/Homework1 febrero")

# Listar los archivos en el directorio de trabajo
dir()

#Cargar los datos y limpieza
df1 <- read_csv("dataset_HW1_insurance.csv")
df2 <- select(df1, -1)
print(df2)
# Contar los valores NA por fila
nas_por_fila <- rowSums(is.na(df2))
# Filtrar las filas que tienen 3 o menos valores NA
DF <- df2[nas_por_fila <= 3, ]
print(DF)

# Encuentra las filas con valores
rows_NA <- apply(DF, 1, function(x) sum(is.na(x)) > 3)
# Selecciona las filas 
DF_with_NAs <- DF[rows_NA, ]
print(DF_with_NAs)

#revisar numeros de filas 
nrow(DF)

# Visualizar conjunto de datos
head(DF)
latex_table <- xtable(head(DF))
print(latex_table, type = "latex", include.rownames = TRUE)

# Verificar datos faltantes
summary(DF)

# Visualizar los datos NA
gg_miss_var(DF)

na_plot <- gg_miss_var(DF)
# Guardar el gráfico como un archivo PNG
ggsave("na_plot.png", na_plot, width = 6, height = 4, dpi = 300)

# Verificar si hay cadenas vacías en todas las columnas del dataframe
sapply(DF, function(x) any(x == ""))
sapply(DF, function(x) any(x == "", na.rm = TRUE))


#Estadísticas descriptivas
#age
tabla_1 <-DF %>%
  dplyr::reframe(
    Prom    = mean(age, na.rm = TRUE),
    Mediana = median(age, na.rm = TRUE),
    Moda = mfv(age, na_rm = TRUE),
    DE      = sd(age, na.rm = TRUE),
    Max     = max(age, na.rm = TRUE),
    Min     = min(age, na.rm = TRUE),
    CV     = sd(age, na.rm = TRUE) / mean(age, na.rm = TRUE) * 100)
print(tabla_1)
write.xlsx(tabla_1, "Tabla_age.xlsx")
library(xtable)
xtable(tabla_1)

#BMI
tabla_2 <- DF %>%
  dplyr::reframe(Prom    = mean(bmi, na.rm = TRUE),
                   Mediana = median(bmi, na.rm = TRUE),
                   Moda = mfv(bmi, na_rm = TRUE),
                   DE      = sd(bmi, na.rm = TRUE),
                   Max     = max(bmi, na.rm = TRUE),
                   Min     = min(bmi, na.rm = TRUE),
                   CV       = sd(bmi, na.rm = TRUE)/mean(bmi, na.rm = TRUE)*100)
print(tabla_2)
write.xlsx(tabla_2, "Tabla_bmi.xlsx")
xtable(tabla_2)

#Children
DF$children <- as.numeric(as.character(DF$children))

tabla_3 <- DF %>%
  dplyr::reframe(
    Prom    = round(mean(children, na.rm = TRUE), 0),
    Mediana = median(children, na.rm = TRUE),
    Moda    = mfv(children, na_rm = TRUE),
    DE      = round(sd(children, na.rm = TRUE), 0),
    Max     = max(children, na.rm = TRUE),
    Min     = min(children, na.rm = TRUE),
    CV      = round(sd(children, na.rm = TRUE) / mean(children, na.rm = TRUE) * 100, 2))
print(tabla_3)
write.xlsx(tabla_3, "Tabla_chi.xlsx")
xtable(tabla_3)

#Charges
tabla_4 <- DF %>%
  dplyr::reframe(Prom    = mean(charges, na.rm = TRUE),
                   Mediana = median(charges, na.rm = TRUE),
                   Moda    = mfv(charges, na_rm = TRUE)[1],
                   DE      = sd(charges, na.rm = TRUE),
                   Max     = max(charges, na.rm = TRUE),
                   Min     = min(charges, na.rm = TRUE),
                   CV       = sd(charges, na.rm = TRUE)/mean(charges, na.rm = TRUE)*100)
print(tabla_4)
write.xlsx(tabla_4, "Tabla_cha.xlsx")
xtable(tabla_4)


#Rango Intercuartilico(IQR) para hayar valores atipicos
#Age
valAtipAge <- DF %>%
  summarise(
    Q1Age = quantile(age, probs = 0.25, na.rm = TRUE),
    Q2Age = quantile(age, probs = 0.5, na.rm = TRUE),
    Q3Age = quantile(age, probs = 0.75, na.rm = TRUE),
    IQRAge = Q3Age - Q1Age,
    LimInfAge = Q1Age - 1.5 * IQRAge,
    LimSupAge = Q3Age + 1.5 * IQRAge)
  # Identificar valores atípicos
    outliersAge <- DF %>%
      filter(age < valAtipAge$LimInfAge | age > valAtipAge$LimSupAge)

# Imprimir los resultados  
print(valAtipAge)
print(outliersAge)
write.xlsx(valAtipAge, "ValAtipAge.xlsx")
xtable(valAtipAge)

#Bmi 
valAtipBmi <- DF %>%
  summarise(
    Q1Bmi = quantile(bmi, probs = 0.25, na.rm = TRUE),
    Q2Bmi = quantile(bmi, probs = 0.5, na.rm = TRUE),
    Q3Bmi = quantile(bmi, probs = 0.75, na.rm = TRUE),
    IQRBmi = Q3Bmi - Q1Bmi,
    LimInfBmi = Q1Bmi - 1.5 * IQRBmi,
    LimSupBmi = Q3Bmi + 1.5 * IQRBmi)
    outliersBmi <- DF %>%
      filter(bmi < valAtipBmi$LimInfBmi | bmi > valAtipBmi$LimSupBmi)
print(valAtipBmi)
print(outliersBmi)
write.xlsx(valAtipBmi, "ValAtipbmi.xlsx")
xtable(valAtipBmi)

#Children
valAtipCHI <- DF %>%
  summarise(
    Q1CHI = quantile(children, probs = 0.25, na.rm = TRUE),
    Q2CHI = quantile(children, probs = 0.5, na.rm = TRUE),
    Q3CHI = quantile(children, probs = 0.75, na.rm = TRUE),
    IQRCHI = Q3CHI - Q1CHI,
    LimInfCHI = Q1CHI - 1.5 * IQRCHI,
    LimSupCHI = Q3CHI + 1.5 * IQRCHI)
      outliersCHI <- DF %>%
         filter(children < valAtipCHI$LimInfCHI | children > valAtipCHI$LimSupCHI)
print(valAtipCHI)
print(outliersCHI)
write.xlsx(valAtipCHI, "ValAtipchi.xlsx")
xtable(valAtipCHI)
      
#Charges
valAtipCHA <- DF %>%
  summarise(
    Q1CHA = quantile(charges, probs = 0.25, na.rm = TRUE),
    Q2CHA = quantile(charges, probs = 0.5, na.rm = TRUE),
    Q3CHA = quantile(charges, probs = 0.75, na.rm = TRUE),
    IQRCHA = Q3CHA - Q1CHA,
    LimInfCHA = Q1CHA - 1.5 * IQRCHA,
    LimSupCHA = Q3CHA + 1.5 * IQRCHA)
    outliersCHA <- DF %>%
       filter(charges < valAtipCHA$LimInfCHA | charges > valAtipCHA$LimSupCHA)
print(valAtipCHA)
print(outliersCHA)
write.xlsx(valAtipCHA, "ValAtipcha.xlsx")
xtable(valAtipCHA)

# Calcular el IQR para la columna
IQR_age <- valAtipAge$IQRAge
IQR_bmi <- valAtipBmi$IQRBmi
IQR_children <- valAtipCHI$IQRCHI
IQR_charges <- valAtipCHA$IQRCHA

# Reemplazar los valores NA con el IQR
DF <- DF %>%
  mutate(
    age = replace(age, is.na(age), IQR_age),
    bmi = replace(bmi, is.na(bmi), IQR_bmi),
    children = replace(children, is.na(children), IQR_children),
    charges = replace(charges, is.na(charges), IQR_charges))

# Columnas cualitativas

moda_fumador <- DF %>%
  filter(!is.na(smoker)) %>%
  count(smoker) %>%
  top_n(1, n) %>%
  pull(smoker)

moda_region <- DF %>%
  filter(!is.na(region)) %>%
  count(region) %>%
  top_n(1, n) %>%
  pull(region)

# Reemplazar los NA con la moda
DF <- DF %>%
  mutate(
    smoker = replace_na(smoker, moda_fumador),
    region = replace_na(region, moda_region))
print(DF)



#Creacion del boxplot con puntos individuales de datos 
#Transformacion del DataFrame a alargo (porque?)
DF_long <- DF %>%
  pivot_longer(
    cols = c(age, bmi,charges), 
    names_to = "variable", 
    values_to = "value" )

# Boxplot

ggplot(DF, aes(x = sex, y = charges)) +
  geom_boxplot() +
  labs(title = "Charges por Sexo", x = "Sexo", y = "Charges")

ggplot(DF, aes(x = smoker, y = charges)) +
  geom_boxplot() +
  labs(title = "Charges por Fumador", x = "Fumador", y = "Charges")

ggplot(DF, aes(x = region, y = charges)) +
  geom_boxplot() +
  labs(title = "Charges por Región", x = "Región", y = "Charges")




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
age_plot1 <- ggplot(data = DF, aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.5) +
  geom_density(color = "blue", alpha = 0.7) +
  labs(title = "Distribución y Densidad de la Edad", x = "Edad", y = "Densidad")
print(age_plot1)

ggsave("age.png", age_plot1, width = 8, height = 6, dpi = 300)

#children
age_plot2 <-ggplot(data = DF, aes(x = children)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", alpha = 0.5) +
  geom_density(color = "darkgreen", alpha = 0.7) +
  labs(title = "Distribución y Densidad del Número de Hijos", x = "Número de Hijos", y = "Densidad")
ggsave("age2.png", age_plot2, width = 8, height = 6, dpi = 300)
print(age_plot2)

#bmi
age_plot3 <-ggplot(data = DF, aes(x = bmi)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "pink", alpha = 0.5) +
  geom_density(color = "red", alpha = 0.7) +
  labs(title = "Distribución y Densidad del BMI", x = "BMI", y = "Densidad")
print(age_plot3)
ggsave("age3.png", age_plot3, width = 8, height = 6, dpi = 300)

#charges
age_plot4 <-ggplot(data = DF, aes(x = charges)) +
  geom_histogram(aes(y = ..density..), binwidth = 500, fill = "purple", alpha = 0.5) +
  geom_density(alpha = 0.7)
print(age_plot4)
ggsave("age4.png", age_plot4, width = 8, height = 6, dpi = 300)

#Gráfico de barras para la variable children 
GBchildren <- DF$children <- factor(DF$children, exclude = NULL)
GBchildren <- ggplot(data = DF, aes(x = children, fill = children)) +
  geom_bar() +
  scale_fill_viridis_d(begin = 0.3, end = 0.9, direction = 1, na.value = "grey") +
  labs(title = "Distribución del Número de Hijos", x = "Número de Hijos", y = "Cantidad") +
  theme_minimal()
print(GBchildren)
ggsave("grbach.png", GBchildren, width = 8, height = 6, dpi = 300)

#CATEGORICAS - CUALITATIVAS

# Gráfico de barras para la variable sex
grbsex <- DF$sex <- factor(DF$sex, exclude = NULL)
grbsex <- ggplot(data = DF, aes(x = sex, fill = sex)) +
  geom_bar() +
  scale_fill_manual(values = c("female" = "pink", "male" = "lightblue", "NA" = "grey"), na.translate = TRUE) +
  labs(title = "Distribución del Sexo", x = "Sexo", y = "Cantidad") +
  theme_minimal()
print(gsex <- DF %>%
  group_by(sex) %>%
  summarise(Frecuencia = n(),
            Proporcion = n() / nrow(DF) * 100))
print(grbsex)
write.xlsx(gsex, "gsex.xlsx")
xtable(gsex)
ggsave("grbsex.png", grbsex, width = 8, height = 6, dpi = 300)

# Gráfico de barras para la variable smoker
grbsmo <- DF$smoker <- factor(DF$smoker, exclude = NULL)
grbsmo <- ggplot(data = DF, aes(x = smoker, fill = smoker)) +
  geom_bar() +
  scale_fill_manual(values = c("yes" = "orange", "no" = "lightgreen", "NA" = "grey"), na.translate = TRUE) +
  labs(title = "Distribución de Fumadores", x = "Fumador", y = "Cantidad") +
  theme_minimal()
print(gsmo <- DF %>%
  group_by(smoker) %>%
  summarise(Frecuencia = n(),
            Proporcion = n() / nrow(DF) * 100))
print(grbsmo)
write.xlsx(gsex, "gsmo.xlsx")
xtable(gsmo)
ggsave("grbsmo.png", grbsmo, width = 8, height = 6, dpi = 300)

# Gráfico de barras para la variable región
grbreg <- DF$region <- factor(DF$region, exclude = NULL)
grbreg <- ggplot(data = DF, aes(x = region, fill = region)) +
  geom_bar() +
  scale_fill_manual(values = c("northeast" = "steelblue", "southeast" = "gold", "southwest" = "coral", "northwest" = "olivedrab", "NA" = "grey"), na.translate = TRUE) +
  labs(title = "Distribución por Región", x = "Región", y = "Cantidad") +
  theme_minimal()
print(greg <- DF %>%
  group_by(region) %>%
  summarise(Frecuencia = n(),
            Proporcion = n() / nrow(DF) * 100))
print(grbreg)
write.xlsx(greg, "greg.xlsx")
xtable(greg)
ggsave("grbreg.png", grbreg, width = 8, height = 6, dpi = 300)

#tabals de contigencia
tabla_contingencia <- table(DF$sex, DF$smoker)
print(tabla_contingencia)


#Asimetría y forma
install.packages("psych")
library(psych)

#Coeficiente de asimetría
skew(DF$age)
skew(DF$bmi)
DF$children <- as.numeric(as.character(DF$children))
skew(DF$children)
skew(DF$charges)

skewness_results <- data.frame(
  Variable = c("Edad", "BMI", "Hijos", "Cargos"),
  Asimetria = c(skew(DF$age), skew(DF$bmi), skew(DF$children), skew(DF$charges)))
#dataframe a tabla
latex_table <- xtable(skewness_results, caption = "Asimetría de las Variables")
print(latex_table, type = "latex", caption.placement = "top", include.rownames = FALSE)


#Coeficiente de curtosis
kurt_age <- kurtosi(DF$age)
kurt_bmi <- kurtosi(DF$bmi)
kurt_children <- kurtosi(DF$children)
kurt_charges <- kurtosi(DF$charges)
print(kurt_age)
print(kurt_bmi)
print(kurt_children)
print(kurt_charges)

curtosis_df <- data.frame(
  Variable = c("Edad", "BMI", "Hijos", "Cargos"),
  Curtosis = c(kurt_age, kurt_bmi, kurt_children, kurt_charges))
curtosis <- xtable(curtosis_df, caption = "Curtosis de las Variables")
print(curtosis, comment = FALSE, booktabs = TRUE)



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

install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)
library(reshape2)
# Asegúrate de tener la matriz de correlación
corr_matrix <- cor(numeric_df, use = "complete.obs")

# Derretir la matriz de correlación
melted_corr <- melt(corr_matrix)

# Crear el gráfico con ggplot2
ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # color define el color de las líneas que separan los cuadros
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  coord_fixed()

install.packages("corrplot")
library(corrplot)
# Asegúrate de tener la matriz de correlación
corr_matrix <- cor(numeric_df, use = "complete.obs")

# Visualizar la matriz de correlación
corrplot(corr_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")



install.packages("fastDummies")
library(fastDummies)

# Convierte variables categóricas a dummies
DF_dummies <- fastDummies::dummy_cols(DF, select_columns = c("sex", "smoker"))

# Ahora DF_dummies tiene columnas adicionales para las variables categóricas
# Puedes seleccionar las variables que quieres incluir en la correlación:
variables_to_correlate <- c("age", "bmi", "children", "charges", "sex_female", "sex_male", "smoker_yes", "smoker_no")
numeric_and_dummies_df <- DF_dummies[variables_to_correlate]

# Calcula la matriz de correlación
corr_matrix <- cor(numeric_and_dummies_df, use = "pairwise.complete.obs")

# Visualiza la matriz de correlación con corrplot
corrplot(corr_matrix, method = "color")

#MODELO

#Multicolinealidad
library("car")

barplot( vif(m3),
         main      = "VIF",
         horiz     = FALSE,
         col       = "steelblue",
         cex.names = 0.8)
abline(h = 2.5, lwd = 2, lty = 2, col='red')

#Modelo de regresi?n cuadr?tica
modelo.cuadratico = lm(n_paquetes ~ precio + precio2)
summary(modelo.cuadratico)

#Grafico de valores estimados
plot(precio, n_paquetes, pch = 19)
points(precio, modelo.precio$fitted.values, col = 2, pch = 19, cex = 2, type = "b")
points(precio, modelo.cuadratico$fitted.values, col = 4, pch = 19, cex = 2, type = "b")

ECM_ml <- sum(modelo.precio$residuals^2)/length(modelo.precio$residuals)
ECM_mc <- sum(modelo.cuadratico$residuals^2)/length(modelo.cuadratico$residuals)

RECM_ml <- sqrt(ECM_ml)
RECM_mc <- sqrt(ECM_mc)

EAM_ml <- sum( abs(modelo.precio$residuals) )/length(modelo.precio$residuals)
EAM_mc <- sum( abs(modelo.cuadratico$residuals) )/length(modelo.cuadratico$residuals)

#Reporte
Resultados = cbind.data.frame("Metrica" = c ("ECM","RECM","EAM"),
                              "Mod_Lineal" = c(ECM_ml,RECM_ml,EAM_ml),
                              "Mod_Cuadratica" = c(ECM_mc,RECM_mc,EAM_mc))
Resultados
#Ajuste de un modelo de regresion multiple
mod.multiple <- lm(y ~ x1+x2+x3+x4, data = Datos_multiple)
summary(mod.multiple)

#Verificar multicolinelidad
library(car)
vif(mod.multiple)


mod.multiple2 <- lm(y ~ x1+x2+x3, data = Datos_multiple)
summary(mod.multiple2)

mod.multiple3 <- lm(y ~ x1+x2, data = Datos_multiple)
summary(mod.multiple3)

mod.multiple4 <- lm(y ~ x2, data = Datos_multiple)
summary(mod.multiple4)


#Validaci?n de supuestos sobre el modelo final
residuales <- mod.multiple4$residuals

#Media cero
t.test(residuales)

#Independencia
#install.packages("randtests")
library(randtests)
runs.test(residuales)

#Normalidad
library(nortest)

ad.test(residuales)
shapiro.test(residuales)


#Homogeneidad de varianzas
install.packages("skedastic")
library(skedastic)

white_lm(mod.multiple4 )
