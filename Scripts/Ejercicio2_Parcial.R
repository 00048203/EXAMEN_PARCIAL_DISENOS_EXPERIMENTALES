########################
# Luis Edmundo Ramírez #
########################

########################
# Carga de Librerías####
########################
library(openxlsx) # Para abrir documentos de Excel
library(tidyverse) # ggplot2, para gráficos
library(moments) # Estadísticos varios
library(nortest) # Test de normalidad no paramétricos
library(lmtest) # Test de normalidad no paramétricos
library(parameters) # Análisis de parámetros ANOVA
library(effectsize)
library(lsr) # Efectos
library(agricolae) # Test Multimedias
library(DescTools) # Test Multimedias
library(pwr2) #Potencia
library(car)
library(gridExtra)
######################
# Carga de datos #####
######################
data.df <- read.xlsx(xlsxFile ="Data/Caso2SS.xlsx",sheet = "Datos")
data.df

##########Visualización de los datos
#data.df['Aleatorio'] <-sample(1:10000,191789,replace=T)
#View(data.df)
#data.df <- data.df[order(data.df$Aleatorio),]
#head(data.df)
#?str: Verificación del dataframe 24obs. y 6 variables.
str(data.df)

# 2. Muestreo

#Creación de números Aleatorios
set.seed(12345)
#install.packages('sampling')
#library(sampling)

estratos <- strata(data.df, stratanames = c('NivelPobreza'), size = c(15,15,15), method = "srswor")
estratos

data.df.muestra <- getdata(data.df, estratos)
View(data.df.muestra)
table(data.df.muestra$NivelPobreza)

#Ordenamos los datos
rows <- sample(nrow(data.df.muestra))
rows
data.df.muestra <- data.df.muestra[rows, ]
View(data.df.muestra)


########################
# Exploración de datos #
########################

table(data.df.muestra$NivelPobreza)
aggregate(Miembros ~ NivelPobreza, data = data.df.muestra, FUN = mean)
aggregate(Miembros ~ NivelPobreza, data = data.df.muestra, FUN = sd)

ggplot(data = data.df.muestra, aes(x = NivelPobreza, y = Miembros, color = NivelPobreza)) +
  geom_boxplot() +  theme_bw()

# Convirtiendo a factor (los levels se pueden modificar dependiendo de los resultados obtenidos)
data.df.muestra$NivelPobreza <- parse_factor(data.df.muestra$NivelPobreza,
                                       levels = c('Extrema',
                                                  'Relativa',
                                                  'No Pobreza'
                                                  ))


#######################
##### ANOVA ###########
#######################
anova <- aov(Miembros ~ NivelPobreza, data = data.df.muestra)
summary(anova)
model_parameters(anova)

anova2 <- aov(Miembros ~ NivelPobreza, data = data.df.muestra)
anova2


eta_squared(anova, partial = FALSE)
etaSquared(anova)

#######################
##### SUPUESTOS #######
#######################
#######################
### 1.NORMALIDAD#######
#######################

#
par(mfrow=c(1,1))

# MÉTODO GRÁFICO
#HISTOGRAMA DE ANOVA DE RESIDUALES
hist(anova$residuals)

#SE PUEDE INDICAR CUÁNTOS GRUPOS SE DESEA CON EL COMANDO BREAK
hist(anova$residuals, breaks = 20)

#GRÁFICA DE LOS PUNTOS  (GRÁFICA Q-Q)
plot(anova, which = 2)

#GRÁFICA DE LOS RESIDUALES SIN LA LÍNEA
qqnorm(anova$residuals)

#GRÁFICA DE LOS RESIDUALES CON LA LÍNEA
qqline(anova$residuals)



#########################################
# PRUEBA DE HIPÓTESIS NO PARAMÉTRICAS####
#########################################

#PRUEBA K-S ("greater") COLA SUPERIOR
#PRUEBA K-S
ks.test(anova$residuals, pnorm, mean(anova$residuals), sd(anova$residuals), alternative = c("greater"))

#PRUEBA SHAPIRO-WILK
#p-valor 0.05 < 0.2497
shapiro.test(anova$residuals)

#PRUEBA ANDERSON-DARLING
#p-valor 0.05 > 0.2908
ad.test(anova$residuals)

########################################
### COMPARACIÓN DE TRATAMIENTOS#######
########################################

############
## Fisher###
## ------###
#(anova, "ambiente", desplegarlo en consola=true)
LSD.test(anova, "NivelPobreza",console=T) 
#Gráfico
plot(LSD.test(anova, "NivelPobreza",console=T))

############
## TUKEY####
## ------###
TukeyHSD(anova) 
#Gráfico
plot(TukeyHSD(anova))

#Tukey HSD Test Con grupos  
HSD.test(anova, "Nivel",console=T)
#Gráfico
plot(HSD.test(anova, "Nivel",console=T))


############
## DUNCAN###
## ------###
duncan.test(anova, "Nivel",console=T) 
#Gráfico
plot(duncan.test(anova, "Nivel",console=T))

############
## NEWMAN###
## ------###
SNK.test(anova, "Nivel", console = T)
#Gráfico
plot(SNK.test(anova, "Nivel", console = T))



######################################
# Modelo de Regresión #
######################################

# Parametros del modelo
# ---------------------
anova2$coefficients
modelo1=lm(Miembros ~ NivelPobreza, data = data.df.muestra, na.action=na.exclude)
summary(modelo1)




