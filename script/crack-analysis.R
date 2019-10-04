library(tidyverse)
library(plyr)
library(igraph)

library(knitr)
library(factoextra)
library(scales)
library(ggplot2)
library(cluster)
library(fpc)

#"di1a", Marihuana
#"di1b", Cocaína
#"di1c", Crack
#"di1d", Alucinógenos
#"di1e", Inhalantes
#"di1f", Heroína
#"di1g", Estimulantes
#"di1h", Ketamina
#"dm1d", Tranquilizante
#"dm1b", Anfetamina
#tb02 tabaco
#al alcohol

data <- read.csv("../data/crack-anylisis.csv", stringsAsFactors = FALSE)
data$X <- NULL


data1 <- data[data$di1c==1,]
data1[data1$desc_ent=="30 VERACRUZ DE IGNACIO DE LA LLAVE",]$desc_ent <- '30 VERACRUZ'
data1[data1$desc_ent=="16 MICHOAC_N DE OCAMPO",]$desc_ent <- '16 MICHOAC_N '

state <- data1$desc_ent


remp.val <- function(variable){
  variable <- ifelse(is.na(variable), -1, variable)
  variable <- ifelse(variable>98, -1, variable)
  return(variable)
}

data1 <- as.data.frame(apply(data1[,-c(2)], 2, remp.val))
data1$desc_ent <- state

#Por sexo
#sort(unique(data1$ds2))
#summary(data1)
sex <- as.factor(mapvalues(data1$ds2, from=c(1,2), to=c("Hombres", "Mujeres")))

#Toma de las edades
#sort(unique(data1$ds3))
#summary(data1)
ages <- cut(data1$ds3, breaks=c(10,20,30,40,50,60,70), 
            labels=c("10-20",
                     "21-30",
                     "31-40",
                     "41-50",
                     "51-60",
                     "61-70"))

#Se considera indigena?
#sort(unique(data1$ds5a))
#summary(data1)
indi <- as.factor(mapvalues(data1$ds5a, from=c(1,2), to=c("Si", "No")))

#Estado civíl
#sort(unique(data1$ds6))
#summary(data1)
status <- as.factor(mapvalues(data1$ds6, from=c(1,2,3,4,5,6), to=c("Casado(a)",
                                                                  "Unión libre",
                                                                  "Separado(a)",
                                                                  "Divorsiado(a)",
                                                                  "Viudo(a)",
                                                                  "Soltero(a)")))

#Religion
#sort(unique(data1$ds7))
#summary(data1)
religion <- as.factor(mapvalues(data1$ds7, from=c(1,2,3,4,5,6), to=c("Católica", 
                                                                    "Protestante/Evangélica",
                                                                    "Judáica",
                                                                    "Cristiana",
                                                                    "Otra",
                                                                    "Ninguna religión")))

#Estudia actualmente?
#sort(unique(data1$ds8))
#summary(data1)
actual_study <- as.factor(mapvalues(data1$ds8, from=c(1,2,3), to=c("No, nunca ha estudiado",
                                                                   "No, pero si ha estudiado",
                                                                   "Si")))

#Ultimo grado de estudios
#sort(unique(data1$ds9))
#summary(data1)
last_study <- as.factor(mapvalues(data1$ds9, from=c(1,2,3,4,5,6,7,8,-1), 
                                  to=c("Primaria incompleta",
                                       "Primaria completa",
                                       "Secundaria incompleta",
                                       "Secundaria completa",
                                       "Bachillerato incompleto",
                                       "Bachillerato completo",
                                       "Licenciatura incompleta",
                                       "Licenciatura completa",
                                       "No contesta")))

#Ha trabajado en los ultimos 30 días
#sort(unique(data1$ds10))
#summary(data1)
did_work <- as.factor(mapvalues(data1$ds10, from=c(1,2), to=c("Si", "No")))

#Por qué motivo no trabaja
#sort(unique(data1$ds14))
#summary(data1)
why_work <- as.factor(mapvalues(data1$ds14, from=c(1,2,3,4,5,7,9,10,12,13,-1), 
                                to=c("Se dedica al hogar", 
                                     "Estudia",
                                     "Es jubilado",
                                     "Incapacidad permanente",
                                     "Es rentista",
                                     "Desempleado",
                                     "Jornada especial",
                                     "Incapacidad temporal",
                                     "Desastre natural",
                                     "Otras razones",
                                     "Si trabaja")))

#Cual es su ocupación
#sort(unique(data1$ds16))
#summary(data1)
work <- as.factor(mapvalues(data1$ds16, from=c(1,2,4,5,6,7,8,9,10,11,12,13,14,15), 
                            to=c("Profesionista", 
                                 "Maestro",
                                 "Microempresario",
                                 "Oficinista",
                                 "Obrero calificado",
                                 "Obrero no calificado",
                                 "Agricultor",
                                 "Campesino",
                                 "Subempleado",
                                 "Estudiante",
                                 "Ama/o de casa",
                                 "Jubilado",
                                 "Incapacidad permanente",
                                 "Otro")))

#Ha tenido hijos
#sort(unique(data1$ds21))
#summary(data1)
sons <- as.factor(mapvalues(data1$ds21, from=c(1,2), to=c("Si", "No")))

#Cuantos
#sort(unique(data1$ds22))
#summary(data1)
much_sons <- as.factor(mapvalues(data1$ds22, from=c(0,1,2,3,4,5,6,-1), 
                                 to=c("No tiene", 
                                      "1",
                                      "2",
                                      "3",
                                      "4",
                                      "5",
                                      "6", 
                                      "Prefiere no decir")))

#Hijos en drogas
#sort(unique(data1$ds23))
#summary(data1)
sons_drugs <- as.factor(mapvalues(data1$ds23, from=c(1,2,-1), 
                                  to=c("Si", 
                                       "No",
                                       "No contesta")))

#A que edad comienza a usar crack
#sort(unique(data1$di4c))
#summary(data1)
ages_cra <- cut(data1$di4c, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                labels=c("No ha usado",
                         "1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61"))

#Tabaco
#sort(unique(data1$tb02))
#summary(data1)
tabaco_any <- as.factor(mapvalues(data1$tb02, from=c(1,2,3,7,9),
                                  to=c("Todos los días",
                                       "Algunos días",
                                       "No fuma actualmente",
                                       "No sabe",
                                       "No responde")))

#edad en que comienza
#sort(unique(data1$tb06))
#summary(data1)
ages_tabaco <- cut(data1$tb06, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                   labels=c("No fuma",
                            "1-10",
                            "11-20",
                            "21-30",
                            "31-40",
                            "41-50",
                            "51-60",
                            "Más de 61"))

#Tranquilizantes
#sort(unique(data1$dm1b))
#summary(data1)
tranq_any <- as.factor(mapvalues(data1$dm1b, from=c(1,2,9),
                                 to=c("Si",
                                      "No",
                                      "No responde/No sabe")))

#Edad tranquilizantes
#sort(unique(data1$dm4b))
#summary(data1)
ages_tranq <- cut(data1$dm4b, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                  labels=c("No ha usado",
                           "1-10",
                           "11-20",
                           "21-30",
                           "31-40",
                           "41-50",
                           "51-60",
                           "Más de 61"))

#Anfetamínicos
#sort(unique(data1$dm1d))
#summary(data1)
anf_any <- as.factor(mapvalues(data1$dm1d, from=c(1,2,9),
                               to=c("Si",
                                    "No",
                                    "No responde/No sabe")))
#edad anfetamínicos
#sort(unique(data1$dm4d))
#summary(data1)
ages_anf <- cut(data1$dm4d, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                labels=c("No ha usado",
                         "1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61"))

#Marihuana
#sort(unique(data1$di1a))
#summary(data1)
mar_any <- as.factor(mapvalues(data1$di1a, from=c(1,2,9),
                               to=c("Si",
                                    "No",
                                    "No responde/No sabe")))

#Edad marihuana
#sort(unique(data1$di4a))
ages_mar <- cut(data1$di4a, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                labels=c("No ha usado",
                         "1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61"))

#cocaína
#sort(unique(data1$di1b))
#summary(data1)
coc_any <- as.factor(mapvalues(data1$di1b, from=c(1,2),
                               to=c("Si",
                                    "No")))

#años cocaína
#sort(unique(data1$di4b))
#summary(data1)
ages_coc <- cut(data1$di4b, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                labels=c("No ha usado",
                         "1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61"))

#Crack
#sort(unique(data1$di1c))
#summary(data1)
#cra_any <- as.factor(mapvalues(data$di1c, from=c(1,2,9),
#                               to=c("Si",
#                                    "No",
#                                    "No responde/No sabe")))

#Crack años
#sort(unique(data1$di4c))
#summary(data1)
ages_cra <- cut(data1$di4c, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                labels=c("No ha usado",
                         "1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61"))

#Alucinógenos
#sort(unique(data1$di1d))
#summary(data1)
alu_any <- as.factor(mapvalues(data1$di1d, from=c(1,2,9),
                               to=c("Si",
                                    "No",
                                    "No responde/No sabe")))

#Alucinógenos años
#sort(unique(data1$di4d))
#summary(data1)
ages_alu <- cut(data1$di4d, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                labels=c("No ha usado",
                         "1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61"))

#Inhalantes
#sort(unique(data1$di1e))
#summary(data1)
ina_any <- as.factor(mapvalues(data1$di1e, from=c(1,2,9),
                               to=c("Si",
                                    "No",
                                    "No responde/No sabe")))

#Inhalanes años
#sort(unique(data1$di4e))
ages_ina <- cut(data1$di4e, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                labels=c("No ha usado",
                         "1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61"))

#Heroína
#sort(unique(data1$di1f))
hero_any <- as.factor(mapvalues(data1$di1f, from=c(1,2),
                                to=c("Si",
                                     "No")))

#Heroína edad
#sort(unique(data1$di4f))
ages_hero <- cut(data1$di4f, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                 labels=c("No ha usado",
                          "1-10",
                          "11-20",
                          "21-30",
                          "31-40",
                          "41-50",
                          "51-60",
                          "Más de 61"))

#extasis
#sort(unique(data1$di1h))
ext_any <- as.factor(mapvalues(data1$di1h, from=c(1,2,9),
                               to=c("Si",
                                    "No",
                                    "No responde/No sabe")))

#extasis edad
#sort(unique(data1$di4h))
ages_ext <- cut(data1$di4h, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                labels=c("No ha usado",
                         "1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61"))

#Alcohol
#sort(unique(data1$al1))
alc_any <- as.factor(mapvalues(data1$al1, from=c(1,2),
                               to=c("Si",
                                    "No")))

#alcohol edad
#sort(unique(data1$al3))
ages_alc <- cut(data1$al3, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                labels=c("No ha usado",
                         "1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61"))

#sort(unique(data1$di3caa))
#uso 1 de 
uso_1 <- as.factor(mapvalues(data1$di3caa, from=c(1,2,3,4,5,6,7,9,-1), 
                                 to=c("Inyectada", 
                                      "Inhalada",
                                      "Aspirada",
                                      "Fumada",
                                      "Tragada",
                                      "Untada",
                                      "No como droga",
                                      "Prefiere no decir", 
                                      "Otra")))


#Cuantas veces
cuantas_veces <- as.factor(mapvalues(data1$di5c, from=c(1,2,3,4,5,-1), 
                             to=c("1-2 veces", 
                                  "3-5 veces",
                                  "6-10 veces",
                                  "11-49 veces",
                                  "50 o más",
                                  "Prefiere no decir")))

#ultimos 30 días
last_30 <- as.factor(mapvalues(data1$di8c, from=c(1,2,3,4,-1), 
                                     to=c("Sí, de 1 a 5 días", 
                                          "Sí, de 6 a 19 días",
                                          "Sí, de 20 a 30 días",
                                          "No",
                                          "Prefiere no decir")))

#ultimos 12 meses
last_12 <- as.factor(mapvalues(data1$di6c, from=c(1,2,-1), 
                               to=c("Sí",
                                    "No",
                                    "Prefiere no decir")))

#Alguna vez el cosumo afecto sus responsabilidades?
prob_1 <- as.factor(mapvalues(data1$ddg1a, from=c(1,2,-1),
                              to=c("Si",
                                   "No",
                                   "No responde")))

#provocó discusión?
prob_2 <- as.factor(mapvalues(data1$ddg2a, from=c(1,2,-1),
                              to=c("Si",
                                   "No",
                                   "No responde")))

#lo hace apesar de
prob_3 <- as.factor(mapvalues(data1$ddg2a1, from=c(1,2,-1),
                              to=c("Si",
                                   "No",
                                   "No responde")))

#uso en situación de peligro
prob_4 <- as.factor(mapvalues(data1$ddg3a, from=c(1,2,-1),
                              to=c("Si",
                                   "No",
                                   "No responde")))

#arrestado
prob_5 <- as.factor(mapvalues(data1$ddg4a, from=c(1,2,-1),
                              to=c("Si",
                                   "No",
                                   "No responde")))

#no se pudo resistir a usar
prob_6 <- as.factor(mapvalues(data1$ddg5a, from=c(1,2,-1),
                              to=c("Si",
                                   "No",
                                   "No responde")))

#aumentar dosis
prob_7 <- as.factor(mapvalues(data1$ddg6a, from=c(1,2,-1),
                              to=c("Si",
                                   "No",
                                   "No responde")))

#sintomas de abstinencia
prob_8 <- as.factor(mapvalues(data1$ddg7a, from=c(1,2,-1),
                              to=c("Si",
                                   "No",
                                   "No responde")))

#uso para evitar
prob_9 <- as.factor(mapvalues(data1$ddg8a, from=c(1,2,-1),
                              to=c("Si",
                                   "No",
                                   "No responde")))

#la uso a pesar de que prometió no hacerlo
prob_10 <- as.factor(mapvalues(data1$ddg9a, from=c(1,2,-1),
                               to=c("Si",
                                    "No",
                                    "No responde")))

# uso más de lo que se propuso
prob_11 <- as.factor(mapvalues(data1$ddg10a, from=c(1,2,-1),
                               to=c("Si",
                                    "No",
                                    "No responde")))

# se dio cuenta de que no podía dejarlo
prob_12 <- as.factor(mapvalues(data1$ddg11a, from=c(1,2,-1),
                               to=c("Si",
                                    "No",
                                    "No responde")))

# Por consumir no tenía tiempo para nada mas
prob_13 <- as.factor(mapvalues(data1$ddg12a, from=c(1,2,-1),
                               to=c("Si",
                                    "No",
                                    "No responde")))

# redujo tiempo para sus actividades por usar sustancia
prob_14 <- as.factor(mapvalues(data1$ddg13a, from=c(1,2,-1),
                               to=c("Si",
                                    "No",
                                    "No responde")))

#a pesar de que tenía un problema de salud
prob_15 <- as.factor(mapvalues(data1$ddg14a, from=c(1,2,-1),
                               to=c("Si",
                                    "No",
                                    "No responde")))

# 3 o mas de las anteriormente mencionadas
prob_16 <- as.factor(mapvalues(data1$ddg15a, from=c(1,2,-1),
                               to=c("Si",
                                    "No",
                                    "No responde")))

#### TRANSICION
data_matrix <- data1[,c("di1a", "di1b", "di1c", "di1d", "di1e", "di1f",
                       "di1g", "di1h", "dm1d", "dm1b")]

data_matrix <- data_matrix[data_matrix$di1a!=9,]
data_matrix <- data_matrix[data_matrix$di1b!=9,]
data_matrix <- data_matrix[data_matrix$di1c!=9,]
data_matrix <- data_matrix[data_matrix$di1d!=9,]
data_matrix <- data_matrix[data_matrix$di1e!=9,]
data_matrix <- data_matrix[data_matrix$di1f!=9,]
data_matrix <- data_matrix[data_matrix$di1g!=9,]
data_matrix <- data_matrix[data_matrix$di1h!=9,]
data_matrix <- data_matrix[data_matrix$dm1d!=9,]
data_matrix <- data_matrix[data_matrix$dm1b!=9,]

data_matrix$di1a[data_matrix$di1a==2] <- 0
data_matrix$di1b[data_matrix$di1b==2] <- 0
data_matrix$di1c[data_matrix$di1c==2] <- 0
data_matrix$di1d[data_matrix$di1d==2] <- 0
data_matrix$di1e[data_matrix$di1e==2] <- 0
data_matrix$di1f[data_matrix$di1f==2] <- 0
data_matrix$di1g[data_matrix$di1g==2] <- 0
data_matrix$di1h[data_matrix$di1h==2] <- 0
data_matrix$dm1d[data_matrix$dm1d==2] <- 0
data_matrix$dm1b[data_matrix$dm1b==2] <- 0

set.seed(1)
names(data_matrix) <- c("Marihuana", "Cocaína", "Crack", "Alucinógeno", "Inhalante",
                        "Heroína", "Estimulantes", "Ketamina", "Opio", "Tranquilizante")
mat <- as.matrix(data_matrix)
adyacency <- t(mat) %*% mat


######################################################################################
#Estados donde se consume
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=data1$desc_ent), fill = "#619DFF",
           stat="count", position="dodge") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Estado",
       y="Cantidad") +
  ggtitle("Estados en los que se consume 'crack'")


#Personas encuestadas por edades
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ages),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ages),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Rango de edades (años)") +
  ggtitle("Personas encuestadas por rango de edad")


#Personas encuestadas que se consideran indigenas
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=indi),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=indi),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="¿Se considera indígena?") +
  ggtitle("Personas encuestadas que se consideran indígenas")

#Personas encuestadas por estado civíl
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=status),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=status),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Estado civíl") +
  ggtitle("Personas encuestadas por estado civíl")

#Personas encuestadas por religión
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=religion),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=religion),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Religión") +
  ggtitle("Personas encuestadas por religión")

#Personas encuestadas por estado actual de estudios
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=actual_study),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=actual_study),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="¿Estudia actualmente?") +
  ggtitle("Personas encuestadas por estado actual de estudios")

#Personas encuestadas por último grado de estudios
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=last_study),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=last_study),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Último grado de estudios alcanzado") +
  ggtitle("Personas encuestadas por último grado de estudios")

#Personas encuestadas que trabajaron en los últimos 30 días
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=did_work),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=did_work),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Trabajó en los últimos 30 días") +
  ggtitle("Personas encuestadas que trabajaron en los últimos 30 días")


#Principal motivo por el que no trabaja
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=why_work),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=why_work),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_color_gradientn(colours = rainbow(5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Motivo") +
  ggtitle("Principal motivo por el que no trabaja")

#Ocupación del encuestado
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=work),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=work),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_color_gradientn(colours = rainbow(5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Ocupación") +
  ggtitle("Ocupación del encuestado")

#El encuestado tiene o ha tenido hijos
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=sons),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=sons),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="¿Tiene o ha tenido hijos?") +
  ggtitle("¿El encuestado tiene o ha tenido hijos?")

#Cuantos hijos viven con usted
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=much_sons),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=much_sons),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="¿Con cuantos hijos vive?") +
  ggtitle("¿Cuantos hijos viven con el encuestado?")

#Alguno de sus hijos tiene problemas con alcohól o drogas
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=sons_drugs),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=sons_drugs),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("Alguno de sus hijos tiene problemas con alcohól o drogas")

#Alguna vez ha usado tabaco
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=tabaco_any),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=tabaco_any),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿El encuestado fuma?")

#Cuando comenzó
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ages_tabaco),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ages_tabaco),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Que edad tenía cuando comienza a fumar?")

#Alguna vez ha usado tranquilizantes
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=tranq_any),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=tranq_any),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿El encuestado ha usado tranquilizantes?")

#Cuando comenzó
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ages_tranq),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ages_tranq),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Que edad tenía cuando comienza a usar tranquilizantes?")

#Alguna vez ha usado anfetamínicos
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=anf_any),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=anf_any),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿El encuestado ha usado anfetamínicos sin prescripción médica?")

#Cuando comenzó
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ages_anf),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ages_anf),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Que edad tenía cuando comienza a usar anfetamínicos sin prescripción médica?")

#Alguna vez ha usado marihuana
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=mar_any),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=mar_any),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿El encuestado ha usado marihuana?")

#Cuando comenzó
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ages_mar),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ages_mar),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Que edad tenía cuando comienza a usar marihuana?")

#Alguna vez ha usado cocaína
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=coc_any),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=coc_any),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿El encuestado ha usado cocaína?")

#Cuando comenzó
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ages_coc),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ages_coc),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Que edad tenía cuando comienza a usar cocaína?")

#Alguna vez ha usado crack
#ggplot(as.data.frame(sex)) +
#  theme_bw()+
#  geom_bar(aes(x=sex, fill=cra_any),
#           stat="count", position="dodge") +
#  geom_text(aes(x=sex, label=..count.., group=cra_any),
#            stat='count',
#            position = position_dodge(width = 1),
#            vjust = -0.5, size = 4) +
#  scale_fill_brewer(palette = "Set3")  +
#  theme(plot.title = element_text(hjust = 0.5)) +
#  labs(x = "Sexo",
#       y="Cantidad",
#       fill="Respuesta") +
#  ggtitle("¿El encuestado ha usado crack?")

#Cuando comenzó
#ggplot(as.data.frame(sex)) +
#  theme_bw()+
#  geom_bar(aes(x=sex, fill=ages_cra),
#           stat="count", position="dodge") +
#  geom_text(aes(x=sex, label=..count.., group=ages_cra),
#            stat='count',
#            position = position_dodge(width = 1),
#            vjust = -0.5, size = 4) +
#  scale_fill_brewer(palette = "Set3")  +
#  theme(plot.title = element_text(hjust = 0.5)) +
#  labs(x = "Sexo",
#       y="Cantidad",
#       fill="Respuesta") +
#  ggtitle("¿Que edad tenía cuando comienza a usar crack?")

#Alguna vez ha usado alu
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=alu_any),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=alu_any),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿El encuestado ha usado alucinógenos?")

#Cuando comenzó
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ages_alu),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ages_alu),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Que edad tenía cuando comienza a usar alucinógenos?")

#Alguna vez ha usado inhalantes
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ina_any),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ina_any),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿El encuestado ha usado inhalantes?")

#Cuando comenzó
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ages_ina),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ages_ina),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Que edad tenía cuando comienza a usar inhalantes?")

#Alguna vez ha usado heroína
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=hero_any),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=hero_any),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿El encuestado ha usado heroína?")

#Cuando comenzó
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ages_hero),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ages_hero),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Que edad tenía cuando comienza a usar heroína?")

#Alguna vez ha usado éxtasis
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ext_any),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ext_any),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿El encuestado ha usado ketamina?")

#Cuando comenzó
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ages_ext),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ages_ext),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Que edad tenía cuando comienza a usar ketamina?")

#Alguna vez ha usado alcohol
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=alc_any),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=alc_any),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿El encuestado ha tomado bebidas alcohólicas?")

#Cuando comenzó alcohol
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ages_alc),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ages_alc),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Que edad tenía cuando comienza a tomar bebídas alcohólicas?")

#Cuando comenzó a usar la droga (principal)
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=ages_cra),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=ages_cra),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Que edad tenía cuando comienza a usar crack?")

#Cuantas veces ha usado
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=cuantas_veces),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=cuantas_veces),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Cuantas veces ha usado 'crack'?")

#Principal uso
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=uso_1),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=uso_1),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Principalmente como ha usado el 'crack'?")

#La ha usado en los últimos 30 días
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=last_30),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=last_30),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Ha usado 'crack' en los últimos 30 días?")

#La ha usado en los últimos 12 meses
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=last_12),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=last_12),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Ha usado 'crack' en los últimos 12 meses?")

#Alguna vez el cosumo afecto sus responsabilidades?
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_1),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_1),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Alguna vez el consumo de 'crack' dificultó sus actividades  
          como estudios, trabajo o en el hogar?")

#provocó discusión?
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_2),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_2),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Alguna vez el consumo de 'crack' le provocó discusiones  
          con familia, amigos, vecinos o compañeros?")

#lo hace apesar de
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_3),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_3),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Siguió consumiendo 'crack' a pesar de dichos problemas?")

#uso en situación de peligro
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_4),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_4),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Ha utilizado 'crack' manejando, en bicicleta
          o en alguna situación parecida?")

#arrestado
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_5),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_5),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Alguna vez ha sido arrestado por su comportamiento
          bajo los efectos del 'crack'?")

#no se pudo resistir a usar
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_6),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_6),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Alguna vez ha tenido tantas ganas de consumir 'crack'
          que no se pudo resistir?")

#aumentar dosis
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_7),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_7),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Alguna vez tuvo que aumentar su dosis para conseguir un efecto
          el cual ya no consiguió con la misma dósis de antes?")

#sintomas de abstinencia
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_8),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_8),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Alguna vez ha sentido malestares como cansancio, temblores o dolores 
            por no consumir 'crack'?")

#uso para evitar
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_9),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_9),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Alguna vez tuvo que usar 'crack' para aliviar dichos sintomas?")

#la uso a pesar de que prometió no hacerlo
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_10),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_10),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Alguna vez consumió 'crack' a pesar de 
          que prometió que ya no lo haría?")

# uso más de lo que se propuso
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_11),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_11),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Alguna vez consumió 'crack' más frecuentemente de 
          lo que se había propuesto?")

# se dio cuenta de que no podía dejarlo
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_12),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_12),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Alguna vez intentó dejar de consumir 'crack' y
          se dió cuenta de que no podía hacerlo?")

# Por consumir no tenía tiempo para nada mas
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_13),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_13),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Alguna vez pasó tanto tiempo consumiendo que se
          dio cuenta que ya no le quedaba tiempo para sus demás
          actividades?")

# redujo tiempo para sus actividades por usar sustancia
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_14),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_14),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Ha tenido temporadas en que el uso de 'crack' hizo
          que disminuyera el tiempo que dedicaba a sus actividades?")

#a pesar de que tenía un problema de salud
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_15),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_15),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Ha usado 'crack' a pesar de tener algun problema de 
          salud grave?")

# 3 o mas de las anteriormente mencionadas
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=prob_16),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=prob_16),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿Alguna vez tuvo 3 o más de las situaciones anteriormente
          mencionadas?")

#Diagrama de transición
g<-graph.adjacency(adyacency, mode="undirected", weighted=TRUE, diag=FALSE)
plot.igraph(g, layout=layout_in_circle, edge.arrow.size=0.1,
            edge.width=(E(g)$weight/max(E(g))),
            edge.label=E(g)$weight,
            edge.color="black",
            edge.curved=.3,
            vertex.label.color="blue",
            edge.label.color="red",
            vertex.size=40,
            vertex.color="yellow",
            main="Transición entre uso de drogas ilegales \n en personas que afirman haber usado 'crack'")

####################################################################################
#Uso para kmedoides
data1$entidad <- NULL
data1$desc_ent <- NULL
data1$di4c.1 <- NULL

#Factorización de sexo
sort(unique(data1$ds2))
str(data1$ds2)
data1$ds2 <- as.numeric(data1$ds2)

#Toma de las edades
sort(unique(data1$ds3))
str(data1$ds3)
data1$ds3 <- as.numeric(cut(data1$ds3, breaks=c(10,20,30,40,50,60,70), 
            labels=c(1,
                     2,
                     3,
                     4,
                     5,
                     6)))

#Se considera indigena?
sort(unique(data1$ds5a))
str(data1$ds5a)
data1$ds5a <- as.numeric(data1$ds5a)

#Estado civíl
sort(unique(data1$ds6))
data1$ds6 <- as.numeric(data1$ds6)

#religion
sort(unique(data1$ds7))
data1$ds7 <- as.numeric(data1$ds7)

#estudia
sort(unique(data1$ds8))
data1$ds8 <- as.numeric(data1$ds8)

#ultimo grado de estudios
sort(unique(data1$ds9))
data1$ds9 <- as.numeric(data1$ds9)

#trabaja
sort(unique(data1$ds10))
data1$ds10 <- as.numeric(data1$ds10)

#por que no trabaja
sort(unique(data1$ds14))
data1$ds14 <- as.numeric(data1$ds14)

#profesion
sort(unique(data1$ds16))
data1$ds16 <- as.numeric(data1$ds16)

#tiene hijos
sort(unique(data1$ds21))
data1$ds21 <- as.numeric(data1$ds21)

#cuantos hijos
sort(unique(data1$ds22))
data1$ds22 <- as.numeric(data1$ds22)

#hijos con drogas
sort(unique(data1$ds23))
data1$ds23 <- as.numeric(data1$ds23)

#ha consumido crack ELIMINAR*********
sort(unique(data1$di1c))
data1$di1c <- as.numeric(data1$di1c)

#cuantos años tenía
sort(unique(data1$di4c))
str(data1$di4c)
data1$di4c <- as.numeric(cut(data1$di4c, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                            labels=c(-1,
                                     1,
                                     2,
                                     3,
                                     4,
                                     5,
                                     6,
                                     7)))

#Edad marihuana
data1$di4a <- as.numeric(cut(data1$di4a, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                             labels=c(-1,
                                      1,
                                      2,
                                      3,
                                      4,
                                      5,
                                      6,
                                      7)))

#edad cocaína
data1$di4b <- as.numeric(cut(data1$di4b, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                             labels=c(-1,
                                      1,
                                      2,
                                      3,
                                      4,
                                      5,
                                      6,
                                      7)))

#edad alucinogenos
data1$di4d <- as.numeric(cut(data1$di4d, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                             labels=c(-1,
                                      1,
                                      2,
                                      3,
                                      4,
                                      5,
                                      6,
                                      7)))

#edad inhalantes
data1$di4e <- as.numeric(cut(data1$di4e, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                             labels=c(-1,
                                      1,
                                      2,
                                      3,
                                      4,
                                      5,
                                      6,
                                      7)))

#edad heroína
data1$di4f <- as.numeric(cut(data1$di4f, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                             labels=c(-1,
                                      1,
                                      2,
                                      3,
                                      4,
                                      5,
                                      6,
                                      7)))

#edad anfetaminas
data1$di4g <- as.numeric(cut(data1$di4g, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                             labels=c(-1,
                                      1,
                                      2,
                                      3,
                                      4,
                                      5,
                                      6,
                                      7)))

#edad ketamina
data1$di4h <- as.numeric(cut(data1$di4h, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                             labels=c(-1,
                                      1,
                                      2,
                                      3,
                                      4,
                                      5,
                                      6,
                                      7)))

#edad alcohol
data1$al3 <- as.numeric(cut(data1$al3, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                             labels=c(-1,
                                      1,
                                      2,
                                      3,
                                      4,
                                      5,
                                      6,
                                      7)))

#edad tabaco
data1$tb06 <- as.numeric(cut(data1$tb06, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                            labels=c(-1,
                                     1,
                                     2,
                                     3,
                                     4,
                                     5,
                                     6,
                                     7)))

#edad Tranquilizantes
data1$dm4b <- as.numeric(cut(data1$dm4b, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                            labels=c(-1,
                                     1,
                                     2,
                                     3,
                                     4,
                                     5,
                                     6,
                                     7)))

#edad anfetaminicos
data1$dm4d <- as.numeric(cut(data1$dm4d, breaks=c(-Inf,0,10,20,30,40,50,60,Inf), 
                             labels=c(-1,
                                      1,
                                      2,
                                      3,
                                      4,
                                      5,
                                      6,
                                      7)))


#di4a, di4b, di4d, di4e, di4f, di4g, di4h, al3
#dm4b, dm4d
#eliminar  di4c.1 

#Se hace K-medoides
data1.resc <- data1
data1.resc$di1c <- NULL

apply(data1.resc, 2, unique)
data1.resc <- as.data.frame(apply(data1.resc, 2, rescale))
data1.resc <- format(round(data1.resc, 2), nsmall = 2)
data1.resc <- as.data.frame(apply(data1.resc, 2, as.numeric))
apply(data1.resc, 2, unique)
set.seed(1)

submt <- kmeans(data1.resc, centers=1)$withinss
for(i in 2:10) submt[i] <- kmeans(data1.resc, centers=i)$withinss

plot(1:10, submt, type="b", xlab="Número de clusters", ylab="Diferencias entre elementos de cada grupo")
abline(v = 4, col="blue", lty=2)

clarafit <- clara(data1.resc, 4, samples = 1000)
#df <- clarafit$medoids
df <- as.data.frame(clarafit$medoids)
#df <- as.data.frame(t(df))

#names(df) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")

df$ds2 <- as.factor(mapvalues(df$ds2, from=c(1, 0), 
                                to=c("Femenino",
                                     "Masculino")))

df$ds3 <- as.factor(mapvalues(df$ds3, from=c(0.4, 0.8, 0.6, 0.2, 0.0, 1.0), 
                              to=c("31-40 años", 
                                   "51-60 años",
                                   "41-50 años",
                                   "21-30 años",
                                   "10-20 años",
                                   "61 años o más")))

df$ds5a <- as.factor(mapvalues(df$ds5a, from=c(0, 1), 
                              to=c("Si", 
                                   "No")))

df$ds6 <- as.factor(mapvalues(df$ds6, from=c(1.0, 0.4, 0.0, 0.2, 0.6, 0.8), 
                               to=c("Soltero/a", 
                                    "Separado/a",
                                    "Casado/a",
                                    "Unión libre",
                                    "Divorciado/a",
                                    "Viudo/a")))

df$ds7 <- as.factor(mapvalues(df$ds7, from=c(0.0, 1.0, 0.6, 0.8, 0.4, 0.2), 
                              to=c("Católica", 
                                   "Ninguna",
                                   "Cristiana",
                                   "Otra",
                                   "Judaica",
                                   "Protestante o evangélica")))

df$ds8 <- as.factor(mapvalues(df$ds8, from=c(0.5, 0.0, 1.0), 
                              to=c("No, pero si ha estudiado", 
                                   "No, nunca ha estudiado",
                                   "Si")))

df$ds9 <- as.factor(mapvalues(df$ds9, from=c(0.56, 0.33, 0.78, 0.44, 0.00,
                                             0.22, 1.00, 0.67, 0.89), 
                              to=c("Secundaria completa", 
                                   "Primaria completa",
                                   "Bachillerato completo",
                                   "Secundaria incompleta",
                                   "No contesta",
                                   "Primaria incompleta",
                                   "Universidad completa",
                                   "Bachillerato incompleto",
                                   "Universidad incompleta")))

df$ds10 <- as.factor(mapvalues(df$ds10, from=c(0, 1), 
                              to=c("Si", 
                                   "No")))

df$ds14 <- as.factor(mapvalues(df$ds14, from=c(0.00, 0.14, 0.57, 0.21, 0.71,
                                               1.00, 0.29, 0.79, 0.93, 0.36, 0.43), 
                               to=c("Si trabaja", 
                                    "Se dedica al hogar",
                                    "Desempleado",
                                    "Estudia",
                                    "Jornada especial",
                                    "Otras razones",
                                    "Es jubilado",
                                    "Incapacidad temporal",
                                    "Desastre natural",
                                    "Incapacidad permanente",
                                    "Es rentista")))

df$ds16 <- as.factor(mapvalues(df$ds16, from=c(0.29, 0.36, 1.00, 0.43, 0.79, 0.57, 0.50, 0.71,
                                               0.64, 0.00, 0.21, 0.86, 0.93, 0.07), 
                               to=c("Oficinista",
                                    "Obrero calificado",
                                    "Otra",
                                    "Obrero no calificado",
                                    "Ama/o de casa",
                                    "Campesino",
                                    "Agricultor",
                                    "Estudiante",
                                    "Subempleado",
                                    "Profesionista",
                                    "Microempresario",
                                    "Jubilado",
                                    "Incapacidad permanente",
                                    "Maestro")))

df$ds21 <- as.factor(mapvalues(df$ds21, from=c(1, 0), 
                               to=c("No", 
                                    "Si")))

df$ds22 <- as.factor(mapvalues(df$ds22, from=c(0.00, 0.14, 0.57, 0.29, 0.43,
                                               0.71, 1.00, 0.86), 
                               to=c("Prefiere no decir", 
                                    "No tiene",
                                    "3",
                                    "1",
                                    "2",
                                    "4",
                                    "6",
                                    "5")))

df$ds23 <- as.factor(mapvalues(df$ds23, from=c(0.00, 0.67, 1.00), 
                               to=c("No contesta", 
                                    "Si",
                                    "No")))

#df$di1c <- as.factor(mapvalues(df$di1c, from=c(0.5), 
#                               to=c("De 10 a 20 años", 
#                                    "De 21 a 30 años")))

df$di3caa <- as.factor(mapvalues(df$di3caa, from=c(0.4, 0.5, 0.3, 0.0, 1.0,
                                                   0.2, 0.6, 0.8, 0.7), 
                               to=c("Aspirada",
                                    "Fumada",
                                    "Inhalada",
                                    "Prefiere no decir",
                                    "No sabe / No contesta",
                                    "Inyectada",
                                    "Tragada",
                                    "No como droga",
                                    "Untada")))

df$di4c <- as.factor(mapvalues(df$di4c, from=c(0.67, 0.33, 0.50, 0.00, 1.00,
                                               0.83, 0.17), 
                                 to=c("41-50", 
                                      "21-30",
                                      "31-40",
                                      "1-10",
                                      "Más de 61",
                                      "51-60",
                                      "11-20")))

df$di5c <- as.factor(mapvalues(df$di5c, from=c(0.33, 0.83, 1.00, 0.67, 0.50, 0.00), 
                               to=c("1-2 veces", 
                                    "11-49 veces",
                                    "50 o más",
                                    "6-10 veces",
                                    "3-5 veces",
                                    "Prefiere no decir")))

df$di6c <- as.factor(mapvalues(df$di6c, from=c(0.00, 0.67, 1.00), 
                               to=c("Prefiere no decir", 
                                    "Si",
                                    "No")))

df$di8c <- as.factor(mapvalues(df$di8c, from=c(1.0, 0.4, 0.0, 0.8, 0.6), 
                               to=c("No", 
                                    "Si, de 1 a 5 veces",
                                    "Prefiere no decir",
                                    "Si, de 20 a más dias",
                                    "Si, de 6 a 19 dias")))

df$ddg1a <- as.factor(mapvalues(df$ddg1a, from=c(0.00, 0.67, 1.00), 
                               to=c("No responde", 
                                    "Si",
                                    "No")))

df$ddg2a <- as.factor(mapvalues(df$ddg2a, from=c(0.00, 0.67, 1.00), 
                                to=c("No responde", 
                                     "Si",
                                     "No")))

df$ddg2a1 <- as.factor(mapvalues(df$ddg2a1, from=c(0.00, 0.67, 1.00), 
                                 to=c("No responde", 
                                      "Si",
                                      "No")))

df$ddg3a <- as.factor(mapvalues(df$ddg3a, from=c(0.00, 1.00, 0.67), 
                                to=c("No responde", 
                                     "No",
                                     "Si")))

df$ddg4a <- as.factor(mapvalues(df$ddg4a, from=c(0.00, 1.00, 0.67), 
                                to=c("No responde", 
                                     "No",
                                     "Si")))

df$ddg5a <- as.factor(mapvalues(df$ddg5a, from=c(0.00, 0.67, 1.00), 
                                to=c("No responde", 
                                     "Si",
                                     "No")))

df$ddg6a <- as.factor(mapvalues(df$ddg6a, from=c(0.00, 1.00, 0.67), 
                                to=c("No responde", 
                                     "No",
                                     "Si")))

df$ddg7a <- as.factor(mapvalues(df$ddg7a, from=c(0.00, 1.00, 0.67), 
                                to=c("No responde", 
                                     "No",
                                     "Si")))

df$ddg8a <- as.factor(mapvalues(df$ddg8a, from=c(0, 1), 
                                to=c("No responde", 
                                     "No")))

df$ddg9a <- as.factor(mapvalues(df$ddg9a, from=c(0.00, 0.67, 1.00), 
                                to=c("No responde", 
                                     "Si",
                                     "No")))

df$ddg10a <- as.factor(mapvalues(df$ddg10a, from=c(0, 1), 
                                 to=c("No responde", 
                                      "No")))

df$ddg11a <- as.factor(mapvalues(df$ddg11a, from=c(0.00, 1.00, 0.67), 
                                 to=c("No responde", 
                                      "No",
                                      "Si")))

df$ddg12a <- as.factor(mapvalues(df$ddg12a, from=c(0.00, 0.67, 1.00), 
                                 to=c("No responde", 
                                      "Si",
                                      "No")))

df$ddg13a <- as.factor(mapvalues(df$ddg13a, from=c(0.00, 1.00, 0.67), 
                                 to=c("No responde", 
                                      "No",
                                      "Si")))

df$ddg14a <- as.factor(mapvalues(df$ddg14a, from=c(0.00, 1.00, 0.67), 
                                 to=c("No responde", 
                                      "No",
                                      "Si")))

df$ddg15a <- as.factor(mapvalues(df$ddg15a, from=c(0.00, 0.67, 1.00), 
                                 to=c("No responde", 
                                      "Si",
                                      "No")))

df$di1a <- as.factor(mapvalues(df$di1a, from=c(0.12, 0.00, 1.00), 
                                 to=c("No", 
                                      "Si",
                                      "No responde/No sabe")))

df$di1b <- as.factor(mapvalues(df$di1b, from=c(1, 0), 
                               to=c("No", 
                                    "Si")))

df$di1d <- as.factor(mapvalues(df$di1d, from=c(0.12, 0.00, 1.00), 
                               to=c("No", 
                                    "Si",
                                    "No responde/No sabe")))

df$di1e <- as.factor(mapvalues(df$di1e, from=c(0.12, 0.00, 1.00), 
                               to=c("No", 
                                    "Si",
                                    "No responde/No sabe")))

df$di1f <- as.factor(mapvalues(df$di1f, from=c(1, 0), 
                               to=c("No", 
                                    "Si")))

df$di1g <- as.factor(mapvalues(df$di1g, from=c(0.12, 0.00, 1.00), 
                               to=c("No", 
                                    "Si",
                                    "No responde/No sabe")))

df$di1h <- as.factor(mapvalues(df$di1h, from=c(0.12, 1.00, 0.00), 
                               to=c("No", 
                                    "No responde/No sabe",
                                    "Si")))

df$di4a <- as.factor(mapvalues(df$di4a, from=c(0.0, 0.4, 0.2, 0.6, 0.8, 1.0), 
                               to=c("1-10", 
                                    "21-30",
                                    "11-20",
                                    "31-40",
                                    "41-50",
                                    "51-60")))

df$di4b <- as.factor(mapvalues(df$di4b, from=c(0.0, 0.4, 0.2, 0.8, 1.0, 0.6), 
                               to=c("1-10", 
                                    "21-30",
                                    "11-20",
                                    "41-50",
                                    "51-60",
                                    "31-40")))

df$di4d <- as.factor(mapvalues(df$di4d, from=c(0.00, 1.00, 0.67, 0.33), 
                               to=c("1-10", 
                                    "31-40",
                                    "21-30",
                                    "11-20")))

df$di4e <- as.factor(mapvalues(df$di4e, from=c(0.00, 1.00, 0.67, 0.33), 
                               to=c("1-10", 
                                    "31-40",
                                    "21-30",
                                    "11-20")))

df$di4f <- as.factor(mapvalues(df$di4f, from=c(0.00, 0.75, 0.50, 0.25, 1.00), 
                               to=c("1-10", 
                                    "31-40",
                                    "21-30",
                                    "11-20",
                                    "41-50")))

df$di4g <- as.factor(mapvalues(df$di4g, from=c(0.00, 0.50, 0.33, 0.67, 1.00, 0.17, 0.83), 
                               to=c("1-10", 
                                    "31-40",
                                    "21-30",
                                    "41-50",
                                    "Más de 61",
                                    "11-20",
                                    "51-60")))

df$di4h <- as.factor(mapvalues(df$di4h, from=c(0.00, 0.50, 0.33, 0.67, 1.00, 0.17, 0.83), 
                               to=c("1-10", 
                                    "31-40",
                                    "21-30",
                                    "41-50",
                                    "Más de 61",
                                    "11-20",
                                    "51-60")))

df$al1 <- as.factor(mapvalues(df$al1, from=c(0, 1), 
                               to=c("Si", 
                                    "No")))

df$al3 <- as.factor(mapvalues(df$al3, from=c(0.00, 0.50, 0.25, 0.75, 1.00), 
                              to=c("1-10", 
                                   "21-30",
                                   "11-20",
                                   "31-40",
                                   "41-50")))

df$dm1b <- as.factor(mapvalues(df$dm1b, from=c(0.12, 0.00, 1.00), 
                              to=c("No", 
                                   "Si",
                                   "No sabe/No responde")))

df$dm1d <- as.factor(mapvalues(df$dm1d, from=c(0.12, 0.00, 1.00), 
                               to=c("No", 
                                    "Si",
                                    "No sabe/No responde")))

df$dm4b <- as.factor(mapvalues(df$dm4b, from=c(0.00, 0.75, 0.50, 1.00, 0.25), 
                               to=c("1-10", 
                                    "31-40",
                                    "21-30",
                                    "41-50",
                                    "11-20")))

df$dm4d <- as.factor(mapvalues(df$dm4d, from=c(0.00, 1.00, 0.67, 0.33), 
                               to=c("1-10", 
                                    "31-40",
                                    "21-30",
                                    "11-20")))

df$tb02 <- as.factor(mapvalues(df$tb02, from=c(0.25, 0.12, 0.00, 1.00), 
                               to=c("No fuma actualmente", 
                                    "Algunos dias",
                                    "Todos los dias",
                                    "No responde")))

df$tb06 <- as.factor(mapvalues(df$tb06, from=c(0.00, 0.50, 0.25, 0.75, 1.00), 
                               to=c("1-10", 
                                    "21-30",
                                    "11-20",
                                    "31-40",
                                    "41-50")))

names(df) <- c("Sexo",
               "Edad",
               "¿Se considera indígena?",
               "Estado civil",
               "Religión",
               "¿Estudia actualmente?",
               "Ultimo grado de estudios",
               "Trabajó en los últimos 30 días",
               "Principal motivo por el que no trabaja",
               "¿Cual es su ocupación?",
               "Ha tenido hijos",
               "Cuantos hijos viven con usted",
               "Tiene hijos con problemas de drogas",
               "¿Como ha usado la sustancia?",
               "¿Qué edad tenía cuando la uso por primera vez?",
               "¿Cuántas veces le ha usado?",
               "¿La ha usado en los últimos 12 meses?",
               "¿La ha usado en los últimos 30 dias?",
               "Alguna vez el cosumo afecto sus responsabilidades?",
               "¿Alguna vez el consumo de 'crack' le provocó discusiones 
                  con familia, amigos, vecinos o compañeros?",
               "¿Siguió consumiendo 'crack' a pesar de dichos problemas?",
               "¿Ha utilizado 'crack' manejando, en bicicleta
          o en alguna situación parecida?",
               "¿Alguna vez ha sido arrestado por su comportamiento
          bajo los efectos del 'crack'?",
               "¿Alguna vez ha tenido tantas ganas de consumir 'crack'
          que no se pudo resistir?",
               "¿Alguna vez tuvo que aumentar su dosis para conseguir un efecto
          el cual ya no consiguió con la misma dósis de antes?",
               "¿Alguna vez ha sentido malestares como cansancio, temblores o dolores 
            por no consumir 'crack'?",
               "¿Alguna vez tuvo que usar 'crack' para aliviar dichos sintomas?",
               "¿Alguna vez consumió 'crack' a pesar de 
          que prometió que ya no lo haría?",
               "¿Alguna vez consumió 'crack' más frecuentemente de 
          lo que se había propuesto?",
               "¿Alguna vez intentó dejar de consumir 'crack' y
          se dió cuenta de que no podía hacerlo?",
               "¿Alguna vez pasó tanto tiempo consumiendo que se
          dio cuenta que ya no le quedaba tiempo para sus demás
          actividades?",
               "¿Ha tenido temporadas en que el uso de 'crack' hizo
          que disminuyera el tiempo que dedicaba a sus actividades?",
               "¿Ha usado 'crack' a pesar de tener algun problema de 
          salud grave?",
               "¿Alguna vez tuvo 3 o más de las situaciones anteriormente
          mencionadas?",
               "¿Ha consumido marihuana?",
               "¿Ha consumido cocaína?",
               "¿Ha consumido alucinógenos?",
               "¿Ha consumido inhalantes?",
               "¿Ha consumido heroína",
               "¿Ha consumido estimulantes",
               "¿ha consumido ketamina",
               "Qué edad tenía cuando comenzó a usar marihuana",
               "¿Qué edad tenía cuando comenzó a usar cocaína?",
               "¿Qué edad tenía cuando comenzó a usar alucinógenos?",
               "¿Qué edad tenía cuando comenzó a usar inhalantes?",
               "¿¿Qué edad tenía cuando comenzó a usar heroína",
               "¿Qué edad tenía cuando comenzó a usar estimulantes",
               "¿Qué edad tenía cuando comenzó a usar ketamina",
               "¿Ha consumido alcohol?",
               "¿Que edad tenía cuando comenzó a usar alcohol?",
               "¿Ha consumido tranquilizantes?",
               "¿Ha consumido anfetamina?",
               "¿Que edad tenía cuando comenzó a usar tranquilizantes?",
               "¿Que edad tenía cuando comenzó a usar anfetamina?",
               "¿Ha consumido tabaco?",
               "¿Que edad tenía cuando comenzó a usar tabaco?")

df <- as.data.frame(t(df))

names(df) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")






