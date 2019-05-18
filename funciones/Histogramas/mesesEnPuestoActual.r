# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

# todos los (meses en el puesto actual >= 360) los vamos a considerar anomalas y vamos a desechar esos registros
encuestas <- encuestas[encuestas$MesesEnElPuestoActual < 480, ]
title <- "Meses en el puesto actual"
xlabel <- "Cantidad de meses"
ylabel <- "Y"



hist(encuestas$MesesEnElPuestoActual, main = title, xlab = xlabel, ylab = ylabel)
rug(encuestas$MesesEnElPuestoActual)