# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

# distribución de sueldo netos por hora en función del puesto
par(mar=c(5,18,1,1))
bymedian <- with(encuestas, reorder(IdPuesto, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")
