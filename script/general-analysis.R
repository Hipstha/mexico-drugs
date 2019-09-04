library(tidyverse)
library(dplyr)

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
  ggtitle("Personas encuestadas por estado civíl")

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

