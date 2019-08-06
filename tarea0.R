#Emmanuel
#06/08/2019

# Subir datos -------------------------------------------------------------

conjunto <- read.csv("C:/MCF/inventario/cuadro1.csv", header = T)
head(conjunto)


# Altura subset -----------------------------------------------------------

H.media <- subset(conjunto, Altura <= mean(conjunto$Altura))

H.16 <- subset(conjunto, Altura < 16.5)


# Vecinos subset ----------------------------------------------------------

Vecinos.3 <- subset(conjunto, Vecinos <= 3)
Vecinos.4 <- subset(conjunto, Vecinos > 4)


# Diametro subset ---------------------------------------------------------

DBHmedia <- subset(conjunto, Diametro < mean(conjunto$Diametro))
DBH.16  <- subset(conjunto, Diametro > 16)


# Especie subset ----------------------------------------------------------

# Cedro rojo
# F Douglasia
# H Suga

EspCedro <- conjunto[(conjunto$Especie == "C"),]
EspRestante <- conjunto[!(conjunto$Especie == "C"),]

DiamCedro16.9 <- subset(EspCedro, Diametro <= 16.9)
AltCedro18.5 <- subset(EspCedro, Altura > 18.5)

DiamRestante16.9 <- subset(EspRestante, Diametro <= 16.9)
AltRestante18.5 <- subset(EspRestante, Altura > 18.5)



# Histogramas Altura -------------------------------------------------------------

hist(conjunto$Altura)
hist(H.media$Altura)
hist(H.16$Altura)


# Histogramas vecinos -----------------------------------------------------


hist(conjunto$Vecinos)
hist(Vecinos.3$Vecinos)
hist(Vecinos.4$Vecinos)

# histogramas diametro ----------------------------------------------------

hist(conjunto$Diametro)
hist(DBHmedia$Diametro)
hist(DBH.16$Diametro)

# estadisticas basicas Altura ---------------------------------------------

mean(conjunto$Altura)
sd(conjunto$Altura)

mean(H.media$Altura)
sd(H.media$Altura)

mean(H.16$Altura)
sd(H.16$Altura)

# estadisticas basicas vecinos --------------------------------------------

mean(conjunto$Vecinos)
sd(conjunto$Vecinos)

mean(Vecinos.3$Vecinos)
sd(Vecinos.3$Vecinos)

mean(Vecinos.4$Vecinos)
sd(Vecinos.4$Vecinos)

# estadisticas basicas diametro -------------------------------------------

mean(conjunto$Diametro)
sd(conjunto$Diametro)

mean(DBHmedia$Diametro)
sd(DBHmedia$Diametro)

mean(DBH.16$Diametro)
sd(DBH.16$Diametro)




