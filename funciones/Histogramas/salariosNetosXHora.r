# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")
# Todos los salarios netos por hora mayores a 1500 los vamos a eliminar por considerarlos anomalos
encuestas <- encuestas[encuestas$SalarioNetoPorHora < 1500, ]
title <- "Salarios neto por hora"
xlabel <- "Salarios"
ylabel <- "Y"

hist(encuestas$SalarioNetoPorHora, main = title, xlab = xlabel, ylab = ylabel)
rug(encuestas$SalarioNetoPorHora)
