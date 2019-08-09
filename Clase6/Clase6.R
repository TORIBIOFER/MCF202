#Emmanuel Ferrer
#Clase 6
#09/08/2019

#Analisis co-varianza
library(repmis)
head(edad)

#Identificar columna SP como factor
edad$SP <-factor(edad$SP)
edad


# separar factor ----------------------------------------------------------

ariz <-subset(edad, SP =="arizonica")
ariz
ariz.lm <-lm(ariz$EDAD ~ ariz$DAP)
ariz.lm

dura <-subset(edad, SP =="durangensis")
dura

# Regresion dos factores --------------------------------------------------

cov.edad <-lm(edad$EDAD ~ edad$DAP + edad$SP)
cov.edad
summary(cov.edad)

#existe diferencia significativas
#p-value que ambas lineas son difenrentes significativas

#aqui se compara en diferentes especies
plot(edad$DAP[edad$SP =="arizonica"], edad$EDAD[edad$SP =="arizonica"],
     col="sky blue", pch="A", xlim=c(0,50),ylim=c(0,130))
abline(cov.edad$coefficients[1], cov.edad$coefficients[2], col="light green")
text(30,20, "Ya = -7.65+1.98*x")


points(edad$DAP[edad$SP =="durangensis"], edad$EDAD[edad$SP =="durangensis"],
       col="blue", pch="D")

abline(cov.edad$coefficients[1] + cov.edad$coefficients[3],
       cov.edad$coefficients[2], col="red", lty="dashed")
text(19,100, "Yd = 11.41+1.98*x")

#hO.nO EXISTE COVARIACION ENTRE LA LINEA DE REGRESION
#h1= EXISTE covaraicion entre la linea de regresion


