# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

# todas las (edad > 65) las vamos a considerar anomalas y vamos a desechar esos registros
encuestas <- encuestas[encuestas$Edad < 66, ]
title <- "Encuesta por Edad"
xlabel <- "Edades"
ylabel <- "Cantidad de encuestas"
hist(encuestas$Edad, main = title, xlab = xlabel, ylab = ylabel)
rug(encuestas$Edad)