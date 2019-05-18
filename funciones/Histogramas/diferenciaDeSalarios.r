# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

# Todas las diferencias de salario real e ideal mayores a 25000 y menores a -5000 las eliminamos
# Por considerarlas anomalas.
encuestas <- encuestas[encuestas$DiferenciaSalarioRealIdeal < 25000, ]
encuestas <- encuestas[encuestas$DiferenciaSalarioRealIdeal > -5000, ]
title <- "Diferencias de salarios"
xlabel <- "Salarios"
ylabel <- "Y"

hist(encuestas$DiferenciaSalarioRealIdeal, main = title, xlab = xlabel, ylab = ylabel)
rug(encuestas$DiferenciaSalarioRealIdeal)