library(knitr)
library(factoextra)
library(scales)
library(ggplot2)
library(cluster)
library(fpc)

data <- read.csv("../data/general-analysis.csv")
data <- data[data$di1a==1 |
               data$di1b==1 |
               data$di1c==1 |
               data$di1d==1 |
               data$di1e==1 |
               data$di1f==1 |
               data$di1g==1 |
               data$di1h==1 |
               data$dm1d==1 |
               data$dm1b==1 |
               data$tb02==1 |
               data$tb02==2,]

##Eliminación de NA
last_study_na_rm <- ifelse(is.na(data$ds9), 99, data$ds9)
why_work_na_rm <- ifelse(is.na(data$ds14), 14, data$ds14)
much_sons_na_rm <- ifelse(is.na(data$ds22), 11, data$ds22)
sons_drugs_na_rm <- ifelse(is.na(data$ds23), 4, data$ds23)
ages_tabaco_na_rm <- ifelse(is.na(data$tb06), 999, data$tb06)
ages_tranq_na_rm <- ifelse(is.na(data$dm4b), 999, data$dm4b)
ages_anf_na_rm <- ifelse(is.na(data$dm4d), 999, data$dm4d)
ages_mar_na_rm <- ifelse(is.na(data$di4a), 999, data$di4a)
ages_coc_na_rm <- ifelse(is.na(data$di4b), 999, data$di4b)
ages_cra_na_rm <- ifelse(is.na(data$di4c), 999, data$di4c)
ages_alu_na_rm <- ifelse(is.na(data$di4d), 999, data$di4d)
ages_ina_na_rm <- ifelse(is.na(data$di4e), 999, data$di4e)
ages_hero_na_rm <- ifelse(is.na(data$di4f), 999, data$di4f)
ages_ext_na_rm <- ifelse(is.na(data$di4h), 999, data$di4h)
ages_alc_na_rm <- ifelse(is.na(data$al3), 999, data$al3)

#Datos para K-Medoides
data_med <- data
data_med$ds9 <- last_study_na_rm
data_med$ds14 <- why_work_na_rm
data_med$ds22 <- much_sons_na_rm
data_med$ds23 <- sons_drugs_na_rm
data_med$tb06 <- ages_tabaco_na_rm
data_med$dm4b <- ages_tranq_na_rm
data_med$dm4d <- ages_anf_na_rm
data_med$di4a <- ages_mar_na_rm
data_med$di4b <- ages_coc_na_rm
data_med$di4c <- ages_cra_na_rm
data_med$di4d <- ages_alu_na_rm
data_med$di4e <- ages_ina_na_rm 
data_med$di4f <- ages_hero_na_rm
data_med$di4h <- ages_ext_na_rm
data_med$al3 <- ages_alc_na_rm
#Toma de las edades
data_med$ds3 <- as.numeric(cut(data_med$ds3, breaks=c(10,20,30,40,50,60,70), 
                               labels=c(1,
                                        2,
                                        3,
                                        4,
                                        5,
                                        6)))
data_med$tb06 <- as.numeric(cut(data_med$tb06, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                                labels=c(1,
                                         2,
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8)))

data_med$dm4b <- as.numeric(cut(data_med$dm4b, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                                labels=c(1,
                                         2,
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8)))
data_med$dm4d <- as.numeric(cut(data_med$dm4d, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                                labels=c(1,
                                         2,
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8)))
data_med$di4a <- as.numeric(cut(data_med$di4a, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                                labels=c(1,
                                         2,
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8)))
data_med$di4b <- as.numeric(cut(data_med$di4b, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                                labels=c(1,
                                         2,
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8)))
data_med$di4c <- as.numeric(cut(data_med$di4c, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                                labels=c(1,
                                         2,
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8)))
data_med$di4d <- as.numeric(cut(data_med$di4d, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                                labels=c(1,
                                         2,
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8)))
data_med$di4e <- as.numeric(cut(data_med$di4e, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                                labels=c(1,
                                         2,
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8)))
data_med$di4f <- as.numeric(cut(data_med$di4f, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                                labels=c(1,
                                         2,
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8)))
data_med$di4h <- as.numeric(cut(data_med$di4h, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                                labels=c(1,
                                         2,
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8)))
data_med$al3 <- as.numeric(cut(data_med$al3, breaks=c(-Inf,10,20,30,40,50,60,70,Inf), 
                               labels=c(1,
                                        2,
                                        3,
                                        4,
                                        5,
                                        6,
                                        7,
                                        8)))

#data_med <- apply(data_med, 2, rescale)

#data <- read.csv("../data/general-analysis-medoids.csv")
#data$X <- NULL

data <- data_med
data$X <- NULL
set.seed(1)

submt <- kmeans(data, centers=1)$withinss
for(i in 2:10) submt[i] <- kmeans(data, centers=i)$withinss

plot(1:10, submt, type="b", xlab="Número de clusters", ylab="Diferencias entre elementos de cada grupo")
abline(v = 3, col="blue", lty=2)

clarafit <- clara(data, 3, samples = 1000)
df <- as.data.frame(clarafit$medoids)

fviz_cluster(clarafit, data=data, geom = "point",
             pointsize = 2, 
             main="Agrupamiento por análisis de componentes principales") + theme_bw()

########################################
set.seed(1)
data_med <- read.csv("../data/general-analysis-medoids.csv")
data_med$X <- NULL
data_med$munici <- NULL
data_med$entidad <- NULL
data_med$ds10 <- NULL
data_med$ds2 <- NULL
data_med$ds3 <- NULL
data_med$ds5a <- NULL
data_med$ds6 <- NULL
data_med$ds7 <- NULL
data_med$ds8 <- NULL
data_med$ds10 <- NULL
data_med$ds16 <- NULL
data_med$ds21 <- NULL
data_med$ds9 <- NULL
data_med$ds14 <- NULL
data_med$ds22 <- NULL
data_med$ds23 <- NULL
submt <- kmeans(data_med, centers=1)$withinss
for(i in 2:10) submt[i] <- kmeans(data_med, centers=i)$withinss

plot(1:10, submt, type="b", xlab="Número de clusters", ylab="Diferencias entre elementos de cada grupo")
abline(v = 6, col="blue", lty=2)

clarafit <- clara(data_med, 7, samples = 1000)
df <- as.data.frame(clarafit$medoids)
df <- t(df)

fviz_cluster(clarafit, data=data_med, geom = "point",
             pointsize = 2, 
             main="Agrupamiento por análisis de componentes principales") + theme_bw()



