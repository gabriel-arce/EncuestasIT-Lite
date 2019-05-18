# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

# todos los (salario actual neto >= 150000) los vamos a considerar anomalas y vamos a desechar esos registros
encuestas <- encuestas[encuestas$SalarioActualNeto >= 150000, ]
title <- "Salario Actual Neto"
xlabel <- "Salarios"
ylabel <- "Y"

hist(encuestas$SalarioActualNeto, main = title, xlab = xlabel, ylab = ylabel)
rug(encuestas$SalarioActualNeto)
