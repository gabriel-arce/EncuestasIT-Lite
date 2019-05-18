# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

#distribución de sueldo netos por hora en función del nivel educativo
par(mar=c(5,15,1,1))
bymedian <- with(encuestas, reorder(IdNivelEducativo, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")