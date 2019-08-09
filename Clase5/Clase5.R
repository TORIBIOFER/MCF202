#Alumno:Emmanuel T. Ferrer
#Matricula:2031741 
#Fecha: 09/08/2019

#Clase No.5

#Analisis de varianza
#comparar efecto 3 concentracion de fertilizantes
#comparar media de los tratamientos
#normalidad de los datos (shapiro)
#que las varianzas sean iguales de los diferentes tratamuento
#ho= que no existe diferencia significativa entre los tratamientos
#h1: al menos una media existe diferencia entre los tratamientos
#Xij= media observacion de los tres tratamientos mas alfa, -error
#i es tratamiento y j numero de observacion
#m es media d ela observacion
#error no explicada
#gl generar niveles (3 tratamientos)


# Ingresar datos para analisis de varianza --------------------------------
arena <- c(6,10,8,6,14,17,9,11,7,11)
arena
arcilla <- c(17,15,3,11,14,12,12,8,10,13)
arcilla
limo <- c(13,16,9,12,15,16,17,13,18,14)
limo

y.ton <-c(arena, arcilla, limo)
y.ton
suelo <-gl(3,10,30,labels = c("arena", "arcilla", "limo"))
suelo
prod <-data.frame(suelo, y.ton)
prod
head(prod)

#media de la productividad para cada uno de los tratamientos
tapply(prod$y.ton, prod$suelo, mean)

#varianza de cada uno de los grupos
tapply(prod$y.ton, prod$suelo, var)

#normalidad de los datos
shapiro.test(prod$y.ton)
#son normales porque es 0.5993 

#varianza prueba comparar
bartlett.test(prod$y.ton, prod$suelo)
#No existe diferencias entre las varianzas
#hay homogeneadad

#determinar homogeneadad de las varianzas
fligner.test(prod$y.ton, prod$suelo)
#no existe diferencia
#hay homogenedad

#no hay tanta variavion entre bartlett vs fligner
#puede ser cualquiera aplica

#inspeccion de los datos
boxplot(prod$y.ton ~ prod$suelo, xlab="Tipo de suelo",
        ylab="Ton/ha", col="light green")

#no hay diferencias significativas, no
#es tan visibles las diferencias

#fuente de variacion es el suelo
#error
#grado de libertad 2
#error= numero de observacion menos tres trataimientos es igua a 27
#total es 29

aov.suelo <- aov(prod$y.ton ~ prod$suelo)
aov.suelo
summary(aov.suelo)

#nos interesa descomponer las varianzas
#cual de los tres bloques es mas variables
#arena es mas variable
#hay que ver si la variacion es entre o fuera
#si hay diferencia entre los tratamientos porque
#dio 0.025 pr(>F)
#son ligeramente significativo
#que procede
#visualizacion

par(mfrow=c(2,2))
par
plot(aov(prod$y.ton ~ prod$suelo))
par(mfrow=c(1,1))
#no hay ningun indicio que existe diferencia
#entre los tratamientos es muy variables
#que son normales

#como mostramos que son dirente a quien
#despues de encontrar diferencia signficativo

#la prueba de tukey (para tres tratamiento o mas)
#para ver que tratamiento es diferente a los otros

TukeyHSD(aov.suelo, conf.level = 0.95)
#arcilla-arena diferencia es 1.6 padj 0.5546301 
#no hay diferencia significativa

#alimo-arena diff  4.4 padj 00.0204414  
#si hay diferencia significativo

#limo-arcilla  dffi 2.8 p adj. 0.1785489
#no hay diferencia significativa

#cual es el mejor tipo de suelo para producir
#limo-arcilla

#graficar
plot(TukeyHSD(aov.suelo))

summary(aov.suelo)
summary.lm(aov.suelo)

#el analisis de varianza es significativa
#p-value: 0.02495

#Descripcion de hipotesis Nula y Alternativas
#Ho.la media de produccion en toneladas 
#es la misma en los tratamientos.

#H1.la media de produccion en toneladas 
#al menos uno es diferente a los demas tratamientos.

















