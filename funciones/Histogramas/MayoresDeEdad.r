# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

# todos los (Edad < 17 y Edad > 0) los vamos a considerar anomalas y vamos a desechar esos registros
encuestas <- encuestas[encuestas$Edad == 0 | encuestas$Edad > 17, ]
title <- "Mayores de edad"
xlabel <- "Edades"
ylabel <- "Y"

hist(encuestas$Edad, main = title, xlab = xlabel, ylab = ylabel)
rug(encuestas$Edad)