
#################################### 
###### SCRIPT DE PREPARACIÓN #######
## Belén Castro y Millaray Maulén ##
####################################

#1. Cargamos los paquetes a utilizar.-

install.packages("pacman")
library(pacman)
p_load(tidyverse, haven, dplyr, car)

#2. Cargamos la base de datos a utilizar.-
base_elsoc <- read_dta("C:/Users/belen/OneDrive/Documentos/GitHub/confianza-medios-edad-educacion/Input/ELSOC_Long_2016_2023.dta")

#2.1 Elegimos la ola 6, que corresponde a la del 2022.-
elsoc2022 <- base_elsoc %>%
  filter(ola == 6)

view(elsoc2022)

#3. Seleccionamos variables.-
elsoc2022_variables <- elsoc2022 %>%
  select(
    sexo = sexo_enc,
    edad   = edad_enc,     
    conf_medios       = c05_12,     
    info_politica_medios = c14_02,
    niveleduc = m01)

#4. Eliminamos los NAs o casos perdidos.-

elsoc2022variables_limpia <- elsoc2022_variables %>%
  mutate(
    edad_= na_if(edad, -888),
    edad = na_if(edad, -999),
    conf_medios = na_if(conf_medios, -888),
    conf_medios = na_if(conf_medios, -999),
    info_politica_medios = na_if(info_politica_medios, -888),
    info_politica_medios = na_if(info_politica_medios, -999),
    niveleduc = na_if(niveleduc, -888),
    niveleduc = na_if(niveleduc, -999)) %>% 
  drop_na(edad, conf_medios, info_politica_medios, niveleduc)   

#5. Recodificamos la variable "EDAD" en grupos etarios.-

elsoc2022_final <- elsoc2022variables_limpia %>%
  mutate(rango_etario = case_when(
    edad >= 18 & edad <= 25 ~ "Jóvenes",
    edad >= 26 & edad <= 59 ~ "Adultos",
    edad >= 60 ~ "Adultos Mayores",
    TRUE ~ NA_character_)) %>%
  mutate(rango_etario = factor(rango_etario, 
                               levels = c("Jóvenes", "Adultos", "Adultos Mayores")))

table(elsoc2022_final$rango_etario)
by(elsoc2022_final$edad, elsoc2022_final$rango_etario, summary)

#6. Recodificamos la variable "NIVEL EDUCACIONAL" en grupos de nivel educacional.-

elsoc22_final <- elsoc2022_final %>%
  mutate(
    nivel_educ = case_when(
      niveleduc <= 3 ~ "Básica",           
      niveleduc >= 4 & niveleduc <= 5 ~ "Media",      
      niveleduc >= 6 & niveleduc <= 10 ~ "Superior",  
      TRUE ~ NA_character_),
    nivel_educ = factor(nivel_educ, levels = c("Básica", "Media", "Superior")))

#7.Descriptivos.-
#7.1 Promedio de confianza según Sexo y Rango Etario.
elsoc22_final %>%
  group_by(sexo, rango_etario) %>%
  summarise(promedio_confianza = mean(conf_medios, na.rm = TRUE),
            desviacion = sd(conf_medios, na.rm = TRUE),
            n = n()) %>%
  ungroup()

#7.2 Promedio de confianza según Nivel Educacional.
elsoc22_final %>%
  group_by(nivel_educ) %>%
  summarise(promedio_confianza = mean(conf_medios, na.rm = TRUE))

#8. Gráficos.

#8.1 Gráfico para Sexo
ggplot(elsoc22_final, aes(x = as.factor(sexo), y = conf_medios, fill = as.factor(sexo))) +
  stat_summary(fun = "mean", geom = "bar") +
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 2)), vjust = -0.5) +
  labs(title = "Promedio de Confianza en Medios de Comunicacion tradicionales según Sexo",
       x = "Sexo (1=Hombre, 2=Mujer)", 
       y = "Promedio de Confianza",
       fill = "Sexo") +
  theme_classic()

#8.2 Gráfico para Rango Etario
ggplot(elsoc22_final, aes(x = rango_etario, y = conf_medios, fill = rango_etario)) +
  stat_summary(fun = "mean", geom = "bar") +
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 2)), vjust = -0.5) +
  labs(title = "Promedio de Confianza en Medios de Comunicacion tradicionales por Rango Etario",
       x = "Grupos de Rango Etario",
       y = "Promedio de Confianza") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

#8.3 Gráfico para Nivel Educacional
ggplot(elsoc22_final, aes(x = nivel_educ, y = conf_medios, fill = nivel_educ)) +
  stat_summary(fun = "mean", geom = "bar") +
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 2)), vjust = -0.5) +
  labs(title = "Promedio de Confianza en Medios de Comunicacion tradicionales por Nivel Educacional",
       x = "Nivel de Educación",
       y = "Promedio de Confianza") +
  theme_light() +
  scale_fill_viridis_d(option = "plasma")
