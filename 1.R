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
library(xtable)

#Fijar el directorio de trabajo
setwd("P:/Estadistica y probabilidad 2/Homework1 febrero")

# Listar los archivos en el directorio de trabajo
dir()

#Cargar los datos
df1 <- read_csv("dataset_HW1_insurance.csv")
df2 <- select(df1, -1)
print(df2)

#revisar numeros de filas 
nrow(df2)

# Visualizar conjunto de datos
head(df2)
latex_table <- xtable(head(df2))
print(latex_table, type = "latex", include.rownames = TRUE)

# Verificar datos faltantes
# Convertir las columnas categóricas a factores
df2$sex <- as.factor(df2$sex)
df2$smoker <- as.factor(df2$smoker)
df2$region <- as.factor(df2$region)
summary(df2)

# Visualizar los datos NA
gg_miss_var(df2)
na_plot <- gg_miss_var(df2)
# Guardar el gráfico como un archivo PNG
ggsave("na_plot.png", na_plot, width = 6, height = 4, dpi = 300)

# Total de valores NA en el dataset
total_na <- df2 %>% 
  summarise_all(~sum(is.na(.))) %>% 
  unlist() %>% 
  sum()
print(total_na)

# Verificar si hay cadenas vacías en todas las columnas del dataframe
sapply(df2, function(x) any(x == "", na.rm = TRUE))


#Estadísticas descriptivas
#age
tabla_1 <-df2 %>%
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
tabla_2 <- df2 %>%
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
tabla_3 <- df2 %>%
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
tabla_4 <- df2 %>%
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

# REMPLAZO DE LOS NA'S PARA LA ANALITICA
moda_sex <- df2 %>%
  filter(!is.na(sex)) %>%
  count(sex) %>%
  top_n(1,n) %>%
  pull(sex)

moda_fumador <- df2 %>%
  filter(!is.na(smoker)) %>%
  count(smoker) %>%
  top_n(1, n) %>%
  pull(smoker)

moda_region <- df2 %>%
  filter(!is.na(region)) %>%
  count(region) %>%
  top_n(1, n) %>%
  pull(region)

moda_children <- df2 %>%
  filter(!is.na(children)) %>%
  count(children) %>%
  top_n(1, n) %>%
  pull(children)

media_age <- mean(DF$age, na.rm = TRUE)
media_bmi <- mean(DF$bmi, na.rm = TRUE)
media_charges <- mean(DF$charges, na.rm = TRUE)

# Reemplazar los NA con la moda,media.
DF <- df2 %>%
  mutate(
    sex = replace_na(sex, moda_sex),
    smoker = replace_na(smoker, moda_fumador),
    region = replace_na(region, moda_region),
    age = ifelse(is.na(age), media_age, age),
    bmi = ifelse(is.na(bmi), media_bmi, bmi),
    children = ifelse(is.na(children), moda_children, children),
    charges = ifelse(is.na(charges), media_charges, charges))
# Visualizar conjunto de datos
head(DF)
latex_table <- xtable(head(DF))
print(latex_table, type = "latex", include.rownames = TRUE)

# Calcular el IQR para la columna
IQR_age <- valAtipAge$IQRAge
IQR_bmi <- valAtipBmi$IQRBmi
IQR_children <- valAtipCHI$IQRCHI
IQR_charges <- valAtipCHA$IQRCHA

# Reemplazar los valores Atipicos con el IQR
DF1 <- DF %>%
  mutate(age = ifelse(age < valAtipAge$LimInfAge, valAtipAge$LimInfAge,
                      ifelse(age > valAtipAge$LimSupAge, valAtipAge$LimSupAge, age)),
         bmi = ifelse(bmi < valAtipBmi$LimInfBmi, valAtipBmi$LimInfBmi,
                      ifelse(bmi > valAtipBmi$LimSupBmi, valAtipBmi$LimSupBmi, bmi)),
         children = ifelse(children < valAtipCHI$LimInfCHI, valAtipCHI$LimInfCHI,
                           ifelse(children > valAtipCHI$LimSupCHI, valAtipCHI$LimSupCHI, children)),
         charges = ifelse(charges < valAtipCHA$LimInfCHA, valAtipCHA$LimInfCHA,
                          ifelse(charges > valAtipCHA$LimSupCHA, valAtipCHA$LimSupCHA, charges)))

write.xlsx(DF1, "DF1.xlsx")    


#Cargar nuevo datset
DF1 <- read.xlsx("DF1.xlsx")

# Boxplot

boxsex <- ggplot(DF1, aes(x = sex, y = charges)) +
  geom_boxplot() +
  labs(title = "Charges- Sex", x = "Sex", y = "Charges")
ggsave("boxsex.png", plot = boxsex, width = 8, height = 6, dpi = 300)

boxsmoker <- ggplot(DF1, aes(x = smoker, y = charges)) +
  geom_boxplot() +
  labs(title = "Charges-smoker", x = "Smoker", y = "Charges")
ggsave("boxsmoker.png", plot = boxsmoker, width = 8, height = 6, dpi = 300)


boxregion <- ggplot(DF1, aes(x = region, y = charges)) +
  geom_boxplot() +
  labs(title = "Charges- Region", x = "Region", y = "Charges")
print(boxregion)
ggsave("boxregion.png", plot = boxregion, width = 8, height = 6, dpi = 300)

boxchildren <- ggplot(DF1, aes(x = factor(children), y = charges)) +
  geom_boxplot() +
  labs(title = "Charges - Children", x = "Children", y = "Charges")
print(boxchildren)
ggsave("boxchildren.png", plot = boxchildren, width = 8, height = 6, dpi = 300)

#Graficos
# Histograma Age
ggplot(DF1, aes(x = age)) + 
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Distribución de la Edad", x = "Edad", y = "Frecuencia")

# Histograma BMI
ggplot(DF1, aes(x = bmi)) + 
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(title = "Distribución del BMI", x = "BMI", y = "Frecuencia")

# Histograma Charges
ggplot(DF1, aes(x = charges)) + 
  geom_histogram(bins = 30, fill = "pink", color = "black") +
  labs(title = "Distribución de los Cargos Médicos", x = "Cargos Médicos", y = "Frecuencia")

# Gráfico de densidad Age
ggplot(DF1, aes(x = age)) + 
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
age_plot1 <- ggplot(data = DF1, aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.5) +
  geom_density(color = "blue", alpha = 0.7) +
  labs(title = "Distribución y Densidad de la Edad", x = "Edad", y = "Densidad")
print(age_plot1)

ggsave("age.png", age_plot1, width = 8, height = 6, dpi = 300)

#children
age_plot2 <-ggplot(data = DF1, aes(x = children)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", alpha = 0.5) +
  geom_density(color = "darkgreen", alpha = 0.7) +
  labs(title = "Distribución y Densidad del Número de Hijos", x = "Número de Hijos", y = "Densidad")
ggsave("age2.png", age_plot2, width = 8, height = 6, dpi = 300)
print(age_plot2)

#bmi
age_plot3 <-ggplot(data = DF1, aes(x = bmi)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "pink", alpha = 0.5) +
  geom_density(color = "red", alpha = 0.7) +
  labs(title = "Distribución y Densidad del BMI", x = "BMI", y = "Densidad")
print(age_plot3)
ggsave("age3.png", age_plot3, width = 8, height = 6, dpi = 300)

#charges
age_plot4 <-ggplot(data = DF1, aes(x = charges)) +
  geom_histogram(aes(y = ..density..), binwidth = 500, fill = "purple", alpha = 0.5) +
  geom_density(alpha = 0.7)
print(age_plot4)
ggsave("age4.png", age_plot4, width = 8, height = 6, dpi = 300)

#Gráfico de barras para la variable children 
GBchildren <- DF1$children <- factor(DF1$children, exclude = NULL)
GBchildren <- ggplot(data = DF1, aes(x = children, fill = children)) +
  geom_bar() +
  scale_fill_viridis_d(begin = 0.3, end = 0.9, direction = 1, na.value = "grey") +
  labs(title = "Distribución de Children", x = "Número de Hijos", y = "Cantidad") +
  theme_minimal()
print(GBchildren)
ggsave("grbach.png", GBchildren, width = 8, height = 6, dpi = 300)
print(greg <- DF1 %>%
        group_by(children) %>%
        summarise(Frecuencia = n(),
                  Proporcion = n() / nrow(DF1) * 100))
print(grbreg)
write.xlsx(greg, "greg.xlsx")
xtable(greg)

#CATEGORICAS - CUALITATIVAS

# Gráfico de barras para la variable sex
grbsex <- DF$sex <- factor(DF1$sex, exclude = NULL)
grbsex <- ggplot(data = DF, aes(x = sex, fill = sex)) +
  geom_bar() +
  scale_fill_manual(values = c("female" = "pink", "male" = "lightblue", "NA" = "grey"), na.translate = TRUE) +
  labs(title = "Distribución del Sexo", x = "Sexo", y = "Cantidad") +
  theme_minimal()
print(gsex <- DF1 %>%
  group_by(sex) %>%
  summarise(Frecuencia = n(),
            Proporcion = n() / nrow(DF1) * 100))
print(grbsex)
write.xlsx(gsex, "gsex.xlsx")
xtable(gsex)
ggsave("grbsex.png", grbsex, width = 8, height = 6, dpi = 300)

# Gráfico de barras para la variable smoker
grbsmo <- DF1$smoker <- factor(DF1$smoker, exclude = NULL)
grbsmo <- ggplot(data = DF, aes(x = smoker, fill = smoker)) +
  geom_bar() +
  scale_fill_manual(values = c("yes" = "orange", "no" = "lightgreen", "NA" = "grey"), na.translate = TRUE) +
  labs(title = "Distribución de Fumadores", x = "Fumador", y = "Cantidad") +
  theme_minimal()
print(gsmo <- DF1 %>%
  group_by(smoker) %>%
  summarise(Frecuencia = n(),
            Proporcion = n() / nrow(DF1) * 100))
print(grbsmo)
write.xlsx(gsmo, "gsmo.xlsx")
xtable(gsmo)
ggsave("grbsmo.png", grbsmo, width = 8, height = 6, dpi = 300)

# Gráfico de barras para la variable región
grbreg <- DF1$region <- factor(DF1$region, exclude = NULL)
grbreg <- ggplot(data = DF, aes(x = region, fill = region)) +
  geom_bar() +
  scale_fill_manual(values = c("northeast" = "steelblue", "southeast" = "gold", "southwest" = "coral", "northwest" = "olivedrab", "NA" = "grey"), na.translate = TRUE) +
  labs(title = "Distribución por Región", x = "Región", y = "Cantidad") +
  theme_minimal()
print(greg <- DF1 %>%
  group_by(region) %>%
  summarise(Frecuencia = n(),
            Proporcion = n() / nrow(DF1) * 100))
print(grbreg)
write.xlsx(greg, "greg.xlsx")
xtable(greg)
ggsave("grbreg.png", grbreg, width = 8, height = 6, dpi = 300)

#tabals de contigencia

# Categorizar 'charges'
DF1$charges_cat <- cut(DF1$charges, breaks = c(-Inf, 10000, 20000, Inf), labels = c("bajo", "medio", "alto"))
# Crear tabla de contingencia
table_smoker_charges <- table(DF1$smoker, DF1$charges_cat)
table_region_charges <- table(DF$region, DF1$charges_cat)
table_sex_charges <- table(DF1$sex, DF1$charges_cat)
DF1$children_cat <- cut(DF1$children, breaks = c(-1, 0, 2, Inf), labels = c("0 hijos", "1-2 hijos", "3+ hijos"))
table_children_charges <- table(DF1$children_cat, DF1$charges_cat)
print(table_smoker_charges)
print(table_sex_charges) 
print(table_children_charges)
write.xlsx(table_smoker_charges, "contigencia-smoker-charges.xlsx")
xtable(table_smoker_charges)
write.xlsx(table_region_charges, "contigencia-region-charges.xlsx")
xtable(table_region_charges)
write.xlsx(table_sex_charges, "contigencia-sex-charges.xlsx")
xtable(table_sex_charges)

#Asimetría y forma
install.packages("psych")
library(psych)

#Coeficiente de asimetría
skew(DF1$age)
skew(DF1$bmi)
DF1$children <- as.numeric(as.character(DF1$children))
skew(DF1$children)
skew(DF1$charges)

skewness_results <- data.frame(
  Variable = c("Edad", "BMI", "Hijos", "Cargos"),
  Asimetria = c(skew(DF1$age), skew(DF1$bmi), skew(DF1$children), skew(DF1$charges)))
#dataframe a tabla
latex_table <- xtable(skewness_results, caption = "Asimetría de las Variables")
print(latex_table, type = "latex", caption.placement = "top", include.rownames = FALSE)


#Coeficiente de curtosis
kurt_age <- kurtosi(DF1$age)
kurt_bmi <- kurtosi(DF1$bmi)
kurt_children <- kurtosi(DF1$children)
kurt_charges <- kurtosi(DF1$charges)
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

windowsFonts()
capabilities()

#Charges y Age
cor_charges_age <- ggplot(DF3, aes(x = age, y = charges)) + 
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
cor_charges_bmi <- ggplot(DF1, aes(x = bmi, y = charges)) + 
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

#charges y children
cor_charges_children <- ggplot(DF1, aes(x = children, y = charges)) + 
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

install.packages("Cairo")
library(Cairo)

# Guardar el gráfico de Edad y Cargos Médicos
Cairo(600, 600, file="cor_charges_age.png", type="png", bg="white", dpi=72, antialias="subpixel")
print(cor_charges_age)
dev.off()

# Guardar el gráfico de BMI y Cargos Médicos
Cairo(600, 600, file="cor_charges_bmi.png", type="png", bg="white", dpi=72, antialias="subpixel")
print(cor_charges_bmi)
dev.off()

# Guardar el gráfico de Número de Hijos y Cargos Médicos
Cairo(600, 600, file="cor_charges_children.png", type="png", bg="white", dpi=72, antialias="subpixel")
print(cor_charges_children)
dev.off()


install.packages("corrplot")
library(corrplot)
library(fastDummies)
# Columnas dummy
DFDUMMY <- dummy_cols(DF1, select_columns = c("sex", "smoker", "region"), remove_selected_columns = TRUE)
head(DFDUMMY)
# Ver el dataframe en ventana 
View(DFDUMMY)

write.xlsx(DFDUMMY, "DFDUMMY.xlsx")
print(DFDUMMY)
# Asumiendo que tu dataframe se llama DF1
p <- head(DF1, 10)
print(xtable(p), type = "latex")


# Calcular la matriz de correlaciones DFDUMMY
corr_matrix <- cor(DFDUMMY, use = "pairwise.complete.obs")
# márgenes y el tamaño de la fuente
par(mar=c(0,0,1,0)) # Puedes ajustar estos números a tu necesidad
corrplot(corr_matrix, method = "color", tl.cex = 0.6, tl.col = "black", cl.cex = 0.7, addgrid.col = "gray")
# Guardar la imagen con un tamaño más grande
png("corrplot_large.png", width = 1200, height = 1200, res = 200)
corrplot(corr_matrix, method = "color", tl.cex = 0.6, tl.col = "black", cl.cex = 0.7, addgrid.col = "gray")
dev.off()

# Columnas numéricas y dummy
numeric <- c("age", "bmi", "children", "charges")
dummy <- c("sex_male", "sex_female", "smoker_yes", "smoker_no", "region_northeast", "region_northwest", "region_southeast", "region_southwest")
# Inicializar el dataframe de interacciones con el mismo número de filas que DFDUMMY
dataInter <- data.frame(matrix(ncol = 0, nrow = nrow(DFDUMMY)))
# Calcular las interacciones entre las columnas dummies y las numéricas
for(num in numeric) {
  for(dum in dummy) {
    interactionName <- paste(dum, num, sep="_")
    dataInter[[interactionName]] <- DFDUMMY[[dum]] * DFDUMMY[[num]]}}
DFFINAL <- cbind(DFDUMMY, dataInter)
print(DFFINAL)
ncol(DFFINAL)

# Calcular la matriz de correlaciones con solo las dummy
corr_matrix1 <- cor(dataInter, use = "pairwise.complete.obs")
# márgenes y el tamaño de la fuente
par(mar=c(0,0,1,0)) 
corrplot(corr_matrix, method = "color", tl.cex = 0.6, tl.col = "black", cl.cex = 0.7, addgrid.col = "gray")
# Guardar la imagen con un tamaño más grande
png("corrplot_large.png", width = 1200, height = 1200, res = 200)
corrplot(corr_matrix, method = "color", tl.cex = 0.6, tl.col = "black", cl.cex = 0.7, addgrid.col = "gray")
dev.off()

# Calcular la matriz de correlaciones las 44 variables
corr_matrix2 <- cor(DFFINAL, use = "pairwise.complete.obs")
# márgenes y el tamaño de la fuente
par(mar=c(0,0,1,0)) 
corrplot(corr_matrix2, method = "color", tl.cex = 0.6, tl.col = "black", cl.cex = 0.7, addgrid.col = "gray")
# Guardar la imagen con un tamaño más grande
png("corrplot_large2.png", width = 1200, height = 1200, res = 200)
corrplot(corr_matrix2, method = "color", tl.cex = 0.6, tl.col = "black", cl.cex = 0.7, addgrid.col = "gray")
dev.off()

colnames(DFFINAL)
install.packages("car")
library(car)
#MODELOq
#Ajuste de modelo
modelos <- lm(charges ~ age+bmi+smoker_no+smoker_yes+sex_male_age+smoker_yes_age+smoker_no_age+region_southeast_age+smoker_yes_bmi+smoker_no_bmi+smoker_yes_children+smoker_no_children, data= DFFINAL)
summary(modelos)
xtable(summary(modelos))

modelo1 <- lm(charges ~ age+bmi+smoker_no+smoker_yes_age+region_southeast_age+smoker_yes_bmi+smoker_yes_children+smoker_no_children, data= DFFINAL)
summary(modelo1)
xtable(summary(modelo1))

modelo2 <- lm(charges ~ age+bmi+smoker_no+smoker_yes_age+smoker_yes_bmi, data= DFFINAL)
summary(modelo2)
xtable(summary(modelo2))

vif_modelo2 <- vif(modelo2)
print(vif_modelo2)
vif_altos <- vif_modelo2[vif_modelo2 > 5]
print(vif_altos)

modelo3 <- lm(charges ~ age + bmi + smoker_yes, data = DFFINAL)
summary(modelo3) #39.08limpia
xtable(summary(modelo3))

modelo4 <- lm(charges ~ smoker_yes_age + smoker_yes_bmi + smoker_no_age + smoker_yes_age, data = DFFINAL )
summary(modelo4)#40.6 limpia
xtable(summary(modelo4))

vif_modelo8 <- vif(modelo8)
print(vif_modelo8)
vif_altos <- vif_modelo8[vif_modelo8 > 5]
print(vif_altos)

modelo5 <- lm(charges ~ age + smoker_yes_age + smoker_yes_bmi, data = DFFINAL )
summary(modelo5)
xtable(summary(modelo5))



#Validación de los supuestos
#modelo4
#Ajuste de un modelo de regresion multiple
modelo4 <- lm(charges ~ smoker_yes_age + smoker_yes_bmi + smoker_no_age + smoker_yes_age, data = DFFINAL )
summary(modelo4)

#Verificar multicolinelidad
library(car)
vif(modelo4)

#Validacion de supuestos sobre el modelo final
residuales4 <- modelo4$residuals
#Media cero
t.test(residuales4)
#Independencia
#install.packages("randtests")
library(randtests)
runs.test(residuales4)
#Normalidad
library(nortest)
ad.test(residuales4)
shapiro.test(residuales4)
#Homogeneidad de varianzas
# Cargar el paquete lmtest si aún no lo has hecho
library(lmtest)
homoge <- bptest(modelo4)
print(homoge)

#Validación de los supuestos
#modelo5
#Ajuste de un modelo de regresion multiple
modelo5 <- lm(charges ~ age + smoker_yes_age + smoker_yes_bmi, data = DFFINAL )
summary(modelo5)

#Verificar multicolinelidad
library(car)
vif(modelo5)

#Validacion de supuestos sobre el modelo final
residuales5 <- modelo5$residuals
#Media cero
t.test(residuales5)
#Independencia
#install.packages("randtests")
library(randtests)
runs.test(residuales5)
#Normalidad
library(nortest)
ad.test(residuales5)
shapiro.test(residuales5)
#Homogeneidad de varianzas
# Cargar el paquete lmtest si aún no lo has hecho
library(lmtest)
homoge <- bptest(modelo5)
print(homoge)