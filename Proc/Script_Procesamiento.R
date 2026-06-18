
#################################### 
###### SCRIPT DE PREPARACIÓN #######
## Belén Castro y Millaray Maulén ##
####################################

#1. Cargamos los paquetes a utilizar.-

install.packages("pacman")
library(pacman)
p_load(tidyverse, haven, dplyr, car, corrplot)

#2. Cargamos la base de datos a utilizar.-
base_elsoc <- read_dta("C:/Users/belen/OneDrive/Documentos/GitHub/confianza-medios-edad-educacion/Input/ELSOC_Long_2016_2023.dta")

#2.1 Elegimos la ola 6, que corresponde a la del 2022.-
elsoc2022 <- base_elsoc %>%
  filter(ola == 6)

view(elsoc2022)

#3. Seleccionamos variables.-
elsoc6variables <- elsoc2022 %>%
  select(
    sexo = sexo_enc,
    edad   = edad_enc,   
    info_politica_medios = c14_02,
    niveleduc = m01,
    sumision_auto = c18_05,
    agresion_auto = c37_05,
    convencionalismo = r12_03)


#4. Eliminamos los NAs o casos perdidos.-

elsoc6limpia <- elsoc6variables %>%
  mutate(
    edad = na_if(edad, -888),
    edad = na_if(edad, -999),
    info_politica_medios = na_if(info_politica_medios, -888),
    info_politica_medios = na_if(info_politica_medios, -999),
    sumision_auto = na_if(sumision_auto, -888),
    sumision_auto = na_if(sumision_auto, -999),
    niveleduc = na_if(niveleduc, -888),
    niveleduc = na_if(niveleduc, -999),
    agresion_auto = na_if(agresion_auto, -888),
    agresion_auto = na_if(agresion_auto, -999),
    convencionalismo = na_if(convencionalismo, -888),
    convencionalismo = na_if(convencionalismo, -999))%>% 
  drop_na(edad,sexo,info_politica_medios,niveleduc,sumision_auto,agresion_auto,convencionalismo)

view(elsoc6limpia)

#5. Recodificamos la variable "EDAD" en grupos etarios.-

elsoc6limpia <- elsoc6limpia %>%
  mutate(
    niveleduc = suppressWarnings(
      as.numeric(as.character(niveleduc))))

elsoc6 <- elsoc6limpia %>%
  mutate(
    
    tramo_edad = case_when(
      edad >= 18 & edad <= 29 ~ "Jóvenes",
      edad >= 30 & edad <= 59 ~ "Adultos",
      edad >= 60 & edad <= 78 ~ "Adultos Mayores",
      TRUE ~ NA_character_),
    
    tramo_edad = factor(
      tramo_edad,
      levels = c(
        "Jóvenes",
        "Adultos",
        "Adultos Mayores")),
    
    niveleducacion = case_when(
      niveleduc >= 1 & niveleduc <= 3 ~ "Básica",
      niveleduc >= 4 & niveleduc <= 5 ~ "Media",
      niveleduc >= 6 & niveleduc <= 10 ~ "Superior",
      TRUE ~ NA_character_))

#7.Tablas.-
#7.1 Tabla de medidas de Tendencia Central
p_load(psych)

tabla_mtc <- describe(elsoc6[, c("sumision_auto", "info_politica_medios", "agresion_auto", "convencionalismo")])

tabla_mtc <- tabla_mtc %>% 
  select(n, mean, sd, median, min, max)

print(tabla_mtc)

#7.2 Tabla de frecuencias.
# Frecuencia para Sexo
tabla_sexo <- elsoc6 %>%
  count(sexo) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Frecuencia para Tramo Edad
tabla_tramoedad <- elsoc6 %>%
  count(tramo_edad) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Frecuencia para Nivel Educacional
tabla_educ <- elsoc6 %>%
  count(niveleducacion) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Ver las tablas
print(tabla_sexo)
print(tabla_tramoedad)
print(tabla_educ)

#8. Gráficos.-

#8.1 Gráfico para Sexo
ggplot(elsoc6, aes(x = as.factor(sexo), y = info_politica_medios, fill = as.factor(sexo))) +
  stat_summary(fun = "mean", geom = "bar") +
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 2)), vjust = -0.5) +
  labs(title = "Promedio de Uso de Medios de Comunicacion para informarse sobre política según Sexo",
       x = "Sexo (1=Hombre, 2=Mujer)", 
       y = "Promedio de uso",
       fill = "Sexo") +
  theme_classic()

#8.2 Gráfico para Rango Etario
ggplot(elsoc6, aes(x = tramo_edad, y = info_politica_medios, fill = tramo_edad)) +
  stat_summary(fun = "mean", geom = "bar") +
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 2)), vjust = -0.5) +
  labs(title = "Promedio de Uso de Medios de Comunicacion para informarse sobre política por Tramo de Edad",
       x = "Tramos Edad",
       y = "Promedio de uso") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

#8.3 Gráfico para Nivel Educacional
ggplot(elsoc6, aes(x = niveleducacion, y = info_politica_medios, fill = niveleducacion)) +
  stat_summary(fun = "mean", geom = "bar") +
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 2)), vjust = -0.5) +
  labs(title = "Promedio de uso de Medios de Comunicacion para informarse sobre politica por Nivel Educacional",
       x = "Nivel de Educación",
       y = "Promedio de uso") +
  theme_light() +
  scale_fill_viridis_d(option = "plasma")

#9. Creación de la variable latente
#9.1 
items_autoritarismo <- elsoc6 %>%
  select(sumision_auto,agresion_auto,convencionalismo,)
#9.2 Sacamos la correlación entre los items
cor(items_autoritarismo, use = "complete.obs")

#9.3 Sacamos el Alfa de Cronbach
check.keys=TRUE
alfa_resultado <- psych::alpha(items_autoritarismo)
alfa_resultado

#10 Correlacion entre nuestra escala y variable indep (informacion politica en medios de comunicacion)

# Reemplazamos los valores -888 y -999 por NA para que no afecten el cálculo
elsoc6$info_politica_medios[elsoc6$info_politica_medios == -888] <- NA
elsoc6$info_politica_medios[elsoc6$info_politica_medios == -999] <- NA

# 10.1 Extraer las puntuaciones promedio creadas por el Alpha de Cronbach
elsoc6$indice_autoritarismo <- alfa_resultado$scores

# 10.2 Corremos la correlación de Spearman
cor.test(
  elsoc6$indice_autoritarismo, 
  elsoc6$info_politica_medios, 
  method = "spearman", 
  use = "complete.obs"
)

matriz_final <- cor(
  elsoc6[, c("indice_autoritarismo", "info_politica_medios")],
  use = "complete.obs",
  method = "spearman"
)

plot(
  elsoc6$info_politica_medios,
  elsoc6$indice_autoritarismo,
  xlab = "Información política por medios",
  ylab = "Índice de autoritarismo",
  main = "Relación entre información política y autoritarismo"
)

abline(
  lm(indice_autoritarismo ~ info_politica_medios, data = elsoc6),
  col = "red",
  lwd = 2
)

# Relación entre nuestra variable independiente y la dependiente (Y)
library(ggplot2)

ggplot(base_regresion1,
       aes(x = info_politica_medios,
           y = indice_autoritarismo)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Información política por medios",
    y = "Índice de autoritarismo"
  )

## Medias condicionales
aggregate(
  indice_autoritarismo ~ info_politica_medios,
  data = base_regresion1,
  mean
)

# 11 Preparación regresion lineal.

## Paquetes a utilizar
pacman::p_load(dplyr, car, sjmisc, sjPlot, sjlabelled, stargazer, kableExtra, corrplot, texreg, ggplot2, ggpubr)

## Base para la regresión.
elsoc6limpia$indice_autoritarismo <- rowMeans(
  items_autoritarismo,
  na.rm = TRUE
)

summary(elsoc6limpia$indice_autoritarismo)

base_regresion1 <- elsoc6limpia[, c(
  "indice_autoritarismo",
  "info_politica_medios"
)]

# 12. Regresión lineal (Modelo 1)

modelolineal1 <- lm(
  indice_autoritarismo ~ info_politica_medios,
  data = base_regresion1
)

summary(modelolineal1)

# 13. Regresión lineal (Modelo 2)

#BASE DATOS
base_regresion2 <- elsoc6limpia[, c(
  "indice_autoritarismo",
  "info_politica_medios",
  "edad",
  "niveleduc"
)]

modelo2 <- lm(
  indice_autoritarismo ~ info_politica_medios +
    edad +
    niveleduc,
  data = base_regresion2
)

summary(modelo2)
  