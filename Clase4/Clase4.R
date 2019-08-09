#Emmanuel Ferrer
#Asignación 1
#09/08/2019
#Clase 4


# Correlacion -------------------------------------------------------------
erupcion <- read.csv("C:/MCF202-2019/Clase4/erupciones.csV", header = T)
summary(erupcion)
plot(erupcion$waiting, erupcion$eruptions,xlab="Tiempo de espera (min)", ylab="Duracion (min)", pch=19, col="light green")

# Determinar la estadistica de las variables ------------------------------
library(pastecs)
stat.desc (erupcion$eruptions)
stat.desc(erupcion$eruptions,basic=FALSE, norm=TRUE)

shapiro.test(erupcion$waiting)
             
cor.test(erupcion$eruptions, erupcion$waiting)

# Conclusion --------------------------------------------------------------
#correlacion significativa
#regresiva

# Regresion lineal --------------------------------------------------------

#Hipotesis nula
# no sirve o significativa la predecir 

#Hipotesis alternativa
#si es significatiba la predecir

#comando "lm" para relaizar la regresion
lm.erup <- lm(erupcion$eruptions ~ erupcion$waiting)
lm.erup
summary(erupcion$eruptions ~ erupcion$waiting)

plot(erupcion$waiting, erupcion$eruptions,xlab="Tiempo de espera (min)", ylab="Duracion (min)", pch=19, col="light green") 
abline(lm.erup, col="red")

lm.erup
summary(lm.erup)

length(erupcion$eruptions)

sqrt(0.90)

(0.90)^2


#coeficiente de regresion
text(52, 4.5, "Y=-1.87 +0.07*x")
text(52, 4, "r^2=0.81")

y.60<- -1.87 + 0.07*60
y.60
-1.87+0.07*30
-1.87+0.07*115

#varianza del experimento
espera <- erupcion$waiting
espera
duracion <- erupcion$eruptions
duracion
res <- resid(lm.erup)
res
sum(res)
pre <-fitted(lm.erup)
pre
res.2 <- res^2
res.2

cuadro <- round(data.frame(espera, duracion, pre, res, res.2),4)
cuadro

SSE <- sum(cuadro$res.2)
SSE

vari <-SSE/(length(erupcion$waiting)-2)
vari

#revision que sean noramles
#que sean independientes
#constancias de alfa beta sean significativo
#establecer el error

# prueba de hipotesis de la regresion -------------------------------------
an.erup <-anova(lm.erup)
an.erup

#regresion es signitifica
#F es 2.2e-16
#aceptamos la alternativa (H1)







# Ejercicio ebano ---------------------------------------------------------

#Base de datos ebano
ebano <- read.csv("C:/MCF202-2019/Clase4/ebanos.csV", header = T)
summary(ebano)
plot(ebano$diametro, ebano$altura, xlab="diametro", ylab="altura", pch=17, col="sky blue")

# Determinar la estadistica de las variables ------------------------------
library(pastecs)
stat.desc (ebano$diametro)
stat.desc(ebano$diametro,basic=FALSE, norm=TRUE)

shapiro.test(log(ebano$diametro))

#porque es la variable dependiente


cor.test(ebano$diametro, ebano$altura)


# Conclusion2 -------------------------------------------------------------

#Los datos no tiene distribucion normal
# Se acepta la H alternativa debido a que el valor de
#p es menor a 2.2e-16 el cual es menor al valor de alfa
#que es 0.05 lo cual quiere decir que si existen diferentes
#significativas (distribucion anormal)


