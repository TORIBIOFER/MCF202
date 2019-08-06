#Emmanuel Ferrer
#06/08/2019
#Clase 2

# Importar datos vivero ---------------------------------------------------

vivero <- read.csv("C:/MCF202-2019/MCF202/Clase2/Clase2.csv", header =T)
summary(vivero)

t.test(vivero$IE, mu=0.85)

# Prueba de t una muestra -------------------------------------------------

par(mfrow=c(1,1))
boxplot(vivero$IE)
t.test(vivero$IE, mu=0.85)
# La media observada no es diferente estadisticamente ya que el valor de p
# es mayor que el alfa establecido (0.05). Ademas la media teórica se
# se encuentra dentro del rango de los valores de intervalos de confianza.

t.test(vivero$IE, mu=0.90)
# La media observada es diferente a la teórica, por lo cual aceptamos
# la H1. 1 valor de p (0.01) es menor que el valor de alfa establecido (0.05).



# Pruebas de t muestras independientes ------------------------------------

boxplot(vivero$IE ~ vivero$Tratamiento, col="green", xlab = "Tratamiento",ylab = "IE" )
shapiro.test(vivero$IE)

#aceptamos la HO. por 0.1777

var.test(vivero$IE ~ vivero$Tratamiento)
#aceptamos la nula por 0.05304
#Las varianzas de ambos tratamientos son iguales asi lo prueba el valor de p
#obtenido mediante una prueba de varianzas (var.test).

t.test(vivero$IE ~ vivero$Tratamiento, var.equal =T)
#Si existe difenrencia rechazamos la hnula y acrptamos la H1
#me conviene fertilizar p-value=0.004868

#El valor de p existe una diferencia significativa entre el IE de las plantulas fertilizadas
#el valor de p (0.4448) comprueba nuestra hipotesis de que el fertilizante "power"
#mejora el IE.