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
data.df <- read.xlsx(xlsxFile ="Data/Caso 1P.xlsx",sheet = "Datos")
data.df

##########Visualización de los datos
data.df['Aleatorio'] <-sample(1:100,24,replace=F)
View(data.df)
data.df <- data.df[order(data.df$Aleatorio),]
head(data.df)

############Cambio de nombre las columnas
colnames(data.df)
colnames(data.df) <- c("Ubicación","Costo_Operación","Costo_Discretizado","Porcentaje_Negocios_Cerrados","Nivel","Orden")

#?str: Verificación del dataframe 24obs. y 6 variables.
str(data.df)

#Ordeno los datos
data.df <- data.df[order(data.df$Orden),]


########################
# Exploración de datos #
########################
#Creación de tabla cruzada de la combinación de niveles de la variable "Ambiente" del dataframe.
table(data.df$Ubicación)
#MEDIA
aggregate(Porcentaje_Negocios_Cerrados ~ Nivel, data = data.df, FUN = mean)
#DESVIACIÓN ESTANDAR
aggregate(Porcentaje_Negocios_Cerrados ~ Nivel, data = data.df, FUN = sd)

ggplot(data = data.df, aes(x = Nivel, y = Porcentaje_Negocios_Cerrados, color = Nivel)) +
  geom_boxplot() + theme_bw()

#ggplot: Función para graficar
#theme_bw(): Controla visualización del gráfico
#geom_boxplot(): Propiedad de Diagrama de cajas y bigotes
?geom_boxplot()

#######################
##### ANOVA ###########
#######################
#DECLARACIÓN DE VARIABLE
anova <- aov(Porcentaje_Negocios_Cerrados ~ Nivel, data = data.df)
anova
anova2 <- aov(Porcentaje_Negocios_Cerrados ~ Costo_Operación, data = data.df)
anova2



#TABLA ANOVA
summary(anova)

#MODELOS DEL PARÁMETRO DE LA ANOVA
model_parameters(anova)

#EXPLICA LA VARIACIÓN Y DA UN INTERVALO DE CONFIANZA
eta_squared(anova, partial = FALSE)

#VARIANZA PARCIAL EXPLICADA POR EL FACTOR SIN INTERVALO DE CONFIANZA
etaSquared(anova)


#######################
##### SUPUESTOS #######
#######################


#######################
### 1.NORMALIDAD#######
#######################

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

#######################
##### ESTADÍSTICOS ####
#######################

#COEFICIENTE DE ASIMETRÍA DE LOS RESIDUALES DE LA ANOVA
#(debe estar entre -1 Y 1)
skewness(anova$residuals)

#CURTOSIS
moments::kurtosis(anova$residuals)

#EXCESO DE CURTOSIS
moments::kurtosis(anova$residuals) - 3


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


##########################
### 2.INDEPENDENCIA#######
##########################
#GRÁFICO DE RESIDUALES DE ANOVA
plot(anova$residuals)

#TEST DURBIN WATSON (ANOVA)
#DW = 2.0075, p-value = 0.5436
dwtest(anova)

#TEST DURBIN WATSON (Autocorrelación)
durbinWatsonTest(anova)

#?bgtest: BREUCH-GODFREY TEST
#bgtest(anova,order = 2)
#LM test = 0.08751, df = 2, p-value = 0.9572
#bgtest(anova,order = 1)
#LM test = 0.031485, df = 1, p-value = 0.8592

#Auto-and-Cross-Covariance and Correlation Function Estimation
acf(anova$residuals, ylim=c(-1,1))


########################################
### 5.COMPARACIÓN DE TRATAMIENTOS#######
########################################

#par(mfrow=c(1,1))

############
## Fisher###
## ------###
#(anova, "ambiente", desplegarlo en consola=true)
LSD.test(anova, "Nivel",console=T) 
#Gráfico
plot(LSD.test(anova, "Nivel",console=T))

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


############
## DUNNET### (un grupo de control)
## ------###
DunnettTest(x=data.df$Porcentaje_Negocios_Cerrados, g=factor(data.df$Nivel))
DunnettTest(x=data.df$Porcentaje_Negocios_Cerrados, g=factor(data.df$Nivel), control = "B")


######################################
# Modelo de Regresión #
######################################

# Parametros del modelo
# ---------------------
anova2$coefficients
modelo1=lm(Porcentaje_Negocios_Cerrados ~ Costo_Operación, data = data.df, na.action=na.exclude)
summary(modelo1)


# Valores estimados
# -----------------
anova2$fitted.values
par(mfrow=c(2,2))
data.df['Estimado'] <- anova2$fitted.values

view(data.df)

plot1 <- ggplot(data = data.df, aes(x = Costo_Operación, y = Porcentaje_Negocios_Cerrados, color = Costo_Operación)) +
  geom_point() + geom_text(label=data.df$Porcentaje_Negocios_Cerrados,nudge_x= 0.5, nudge_y= 0.5)

plot2 <- ggplot(data = data.df, aes(x = Costo_Operación, y = Estimado, color = Costo_Operación)) +
  geom_point() + geom_text(label=data.df$Estimado,nudge_x= 0.5, nudge_y= 0.5)

grid.arrange(plot1, plot2, ncol=2)

