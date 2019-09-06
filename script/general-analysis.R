library(tidyverse)
library(plyr)

data <- read.csv("../data/general-analysis.csv")

#Factorización de sexo
sex <- as.factor(mapvalues(data$ds2, from=c(1,2), to=c("Hombres", "Mujeres")))

#Toma de las edades
ages <- cut(data$ds3, breaks=c(10,20,30,40,50,60,70), 
            labels=c("10-20",
                     "21-30",
                     "31-40",
                     "41-50",
                     "51-60",
                     "61-70"))
#Se considera indigena?
indi <- as.factor(mapvalues(data$ds5a, from=c(1,2), to=c("Si", "No")))
#Estado civíl
status <- as.factor(mapvalues(data$ds6, from=c(1,2,3,4,5,6), to=c("Casado(a)",
                                                                  "Unión libre",
                                                                  "Separado(a)",
                                                                  "Divorsiado(a)",
                                                                  "Viudo(a)",
                                                                  "Soltero(a)")))

religion <- as.factor(mapvalues(data$ds7, from=c(1,2,3,4,5,6), to=c("Católica", 
                                                                    "Protestante/Evangélica",
                                                                    "Judáica",
                                                                    "Cristiana",
                                                                    "Otra",
                                                                    "Ninguna religión")))

actual_study <- as.factor(mapvalues(data$ds8, from=c(1,2, 3), to=c("No, nunca ha estudiado",
                                                                   "No, pero si ha estudiado",
                                                                   "Si")))

last_study_na_rm <- ifelse(is.na(data$ds9), 99, data$ds9)

last_study <- as.factor(mapvalues(last_study_na_rm, from=c(1,2,3,4,5,6,7,8,9,99), 
                                  to=c("Primaria incompleta",
                                       "Primaria completa",
                                       "Secundaria incompleta",
                                       "Secundaria completa",
                                       "Bachillerato incompleto",
                                       "Bachillerato completo",
                                       "Licenciatura incompleta",
                                       "Licenciatura completa",
                                       "Postgrado",
                                       "No contesta")))

did_work <- as.factor(mapvalues(data$ds10, from=c(1,2), to=c("Si", "No")))

why_work_na_rm <- ifelse(is.na(data$ds14), 14, data$ds14)
why_work <- as.factor(mapvalues(why_work_na_rm, from=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), 
                                to=c("Se dedica al hogar", 
                                     "Estudia",
                                     "Es jubilado",
                                     "Incapacidad permanente",
                                     "Es rentista",
                                     "Despido",
                                     "Desempleado",
                                     "Renuncia",
                                     "Jornada especial",
                                     "Incapacidad temporal",
                                     "Vacaciones",
                                     "Desastre natural",
                                     "Otras razones",
                                     "Si trabaja")))

work <- as.factor(mapvalues(data$ds16, from=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14, 15), 
                            to=c("Profesionista", 
                                 "Maestro",
                                 "Empresario",
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

sons <- as.factor(mapvalues(data$ds21, from=c(1,2), to=c("Si", "No")))

much_sons_na_rm <- ifelse(is.na(data$ds22), 11, data$ds22)
much_sons <- as.factor(mapvalues(much_sons_na_rm, from=c(0,1,2,3,4,5,6,7,8,9,10,11), 
                                 to=c("No tiene", 
                                      "1",
                                      "2",
                                      "3",
                                      "4",
                                      "5",
                                      "6", 
                                      "7", 
                                      "8", 
                                      "9", 
                                      "10 o más", 
                                      "Prefiere no decir")))

sons_drugs_na_rm <- ifelse(is.na(data$ds23), 4, data$ds23)
sons_drugs <- as.factor(mapvalues(sons_drugs_na_rm, from=c(1,2,4), 
                                  to=c("Si", 
                                       "No",
                                       "No contesta")))

#Tabaco
tabaco_any <- as.factor(mapvalues(data$tb02, from=c(1,2,3,7,9),
                                  to=c("Todos los días",
                                       "Algunos días",
                                       "No fuma actualmente",
                                       "No sabe",
                                       "No responde")))
ages_tabaco_na_rm <- ifelse(is.na(data$tb06), 999, data$tb06)
ages_tabaco <- cut(ages_tabaco_na_rm, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                   labels=c("1-10",
                            "11-20",
                            "21-30",
                            "31-40",
                            "41-50",
                            "51-60",
                            "Más de 61",
                            "No fuma"))

#Tranquilizantes
tranq_any <- as.factor(mapvalues(data$dm1b, from=c(1,2,9),
                                 to=c("Si",
                                      "No",
                                      "No responde/No sabe")))
ages_tranq_na_rm <- ifelse(is.na(data$dm4b), 999, data$dm4b)
ages_tranq <- cut(ages_tranq_na_rm, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                  labels=c("1-10",
                           "11-20",
                           "21-30",
                           "31-40",
                           "41-50",
                           "51-60",
                           "Más de 61",
                           "No ha usado"))

#Anfetamínicos
anf_any <- as.factor(mapvalues(data$dm1d, from=c(1,2,9),
                               to=c("Si",
                                    "No",
                                    "No responde/No sabe")))
ages_anf_na_rm <- ifelse(is.na(data$dm4d), 999, data$dm4d)
ages_anf <- cut(ages_anf_na_rm, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                labels=c("1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61",
                         "No ha usado"))

#Marihuana
mar_any <- as.factor(mapvalues(data$di1a, from=c(1,2,9),
                               to=c("Si",
                                    "No",
                                    "No responde/No sabe")))
ages_mar_na_rm <- ifelse(is.na(data$di4a), 999, data$di4a)
ages_mar <- cut(ages_mar_na_rm, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                labels=c("1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61",
                         "No ha usado"))

#cocaína
coc_any <- as.factor(mapvalues(data$di1b, from=c(1,2,9),
                               to=c("Si",
                                    "No",
                                    "No responde/No sabe")))
ages_coc_na_rm <- ifelse(is.na(data$di4b), 999, data$di4b)
ages_coc <- cut(ages_coc_na_rm, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                labels=c("1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61",
                         "No ha usado"))

#Crack
cra_any <- as.factor(mapvalues(data$di1c, from=c(1,2,9),
                               to=c("Si",
                                    "No",
                                    "No responde/No sabe")))
ages_cra_na_rm <- ifelse(is.na(data$di4c), 999, data$di4c)
ages_cra <- cut(ages_cra_na_rm, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                labels=c("1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61",
                         "No ha usado"))

#Alucinógenos
alu_any <- as.factor(mapvalues(data$di1d, from=c(1,2,9),
                               to=c("Si",
                                    "No",
                                    "No responde/No sabe")))
ages_alu_na_rm <- ifelse(is.na(data$di4d), 999, data$di4d)
ages_alu <- cut(ages_alu_na_rm, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                labels=c("1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61",
                         "No ha usado"))

#Inhalantes
ina_any <- as.factor(mapvalues(data$di1e, from=c(1,2,9),
                               to=c("Si",
                                    "No",
                                    "No responde/No sabe")))
ages_ina_na_rm <- ifelse(is.na(data$di4e), 999, data$di4e)
ages_ina <- cut(ages_ina_na_rm, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                labels=c("1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61",
                         "No ha usado"))

#Heroína
hero_any <- as.factor(mapvalues(data$di1f, from=c(1,2,9),
                                to=c("Si",
                                     "No",
                                     "No responde/No sabe")))
ages_hero_na_rm <- ifelse(is.na(data$di4f), 999, data$di4f)
ages_hero <- cut(ages_hero_na_rm, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                 labels=c("1-10",
                          "11-20",
                          "21-30",
                          "31-40",
                          "41-50",
                          "51-60",
                          "Más de 61",
                          "No ha usado"))

#ext
ext_any <- as.factor(mapvalues(data$di1h, from=c(1,2,9),
                               to=c("Si",
                                    "No",
                                    "No responde/No sabe")))
ages_ext_na_rm <- ifelse(is.na(data$di4h), 999, data$di4h)
ages_ext <- cut(ages_ext_na_rm, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                labels=c("1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61",
                         "No ha usado"))

#Alcohol
alc_any <- as.factor(mapvalues(data$al1, from=c(1,2),
                               to=c("Si",
                                    "No")))
ages_alc_na_rm <- ifelse(is.na(data$al3), 999, data$al3)
ages_alc <- cut(ages_alc_na_rm, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                labels=c("1-10",
                         "11-20",
                         "21-30",
                         "31-40",
                         "41-50",
                         "51-60",
                         "Más de 61",
                         "No ha tomado"))

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
  ggtitle("Personas encuestadas por estado civíl")

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
ggplot(as.data.frame(sex)) +
  theme_bw()+
  geom_bar(aes(x=sex, fill=cra_any),
           stat="count", position="dodge") +
  geom_text(aes(x=sex, label=..count.., group=cra_any),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sexo",
       y="Cantidad",
       fill="Respuesta") +
  ggtitle("¿El encuestado ha usado crack?")

#Cuando comenzó
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

#Alguna vez ha usado éxtasis
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

#Cuando comenzó
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
  ggtitle("¿Que edad tenía cuando comienza a tomar?")

