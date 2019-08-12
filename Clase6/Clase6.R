#Emmanuel Ferrer
#Clase 6
#09/08/2019

#Analisis co-varianza

rascon <-read.csv("C:/MCF202-2019/rascon.csv", header= T)
head(rascon)
summary(rascon)

# Actividad a realizar ----------------------------------------------------
#Estadistica descriptiva
mean(rascon$DAP)
mean(rascon$EDAD)

sd(rascon$DAP)
sd(rascon$EDAD)

var(rascon$DAP)
var(rascon$EDAD)

# Correlacion -------------------------------------------------------------
shapiro.test(rascon$EDAD)

shapiro.test(rascon$DAP)

cor.test(rascon$EDAD, rascon$DAP)

# Conclusion de correlacion -----------------------------------------------
#la correlacion r=0.7953145

#correlacion: 
#la correlacion entre las variables es siginificativa porque se
#observa que el valor de p-value < 3.201e-14.
#cuales el coeficiente de correlacion (r)?


# Analisis de covarianza --------------------------------------------------
lm.rasco <- lm(rascon$EDAD ~ rascon$DAP)
lm.rasco
summary(lm.rasco)
length(rascon$EDAD)

# dentificar columna SP como factor
arizedad <- factor(rascon$SP)
arizedad

# separar factor
ariz <-subset(rascon, SP =="arizonica")
ariz

ariz.lm <-lm(ariz$EDAD ~ ariz$DAP)
ariz.lm

dura <-subset(rascon, SP =="durangensis")
dura

# Regresion dos factores 
cov.edad <-lm(rascon$EDAD ~ rascon$DAP + rascon$SP)
cov.edad
summary(cov.edad)

#existe diferencia significativas
#p-value que ambas lineas son difenrentes significativas

#aqui se compara en diferentes especies
plot(rascon$DAP[rascon$SP =="arizonica"], rascon$EDAD[rascon$SP =="arizonica"],
     col="sky blue", pch="A", xlim=c(0,50),ylim=c(0,130))
abline(cov.edad$coefficients[1], cov.edad$coefficients[2], col="light green")
text(30,20, "Ya = -7.65+1.98*x")


points(rascon$DAP[rascon$SP =="durangensis"], rascon$EDAD[rascon$SP =="durangensis"],
       col="blue", pch="D")

abline(cov.edad$coefficients[1] + cov.edad$coefficients[3],
       cov.edad$coefficients[2], col="red", lty="dashed")
text(19,100, "Yd = 11.41+1.98*x")

#hO.nO EXISTE COVARIACION ENTRE LA LINEA DE REGRESION
#h1= EXISTE covaraicion entre la linea de regresion


