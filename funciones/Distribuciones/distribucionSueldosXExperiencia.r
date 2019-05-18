# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

# distribución de sueldo netos por hora en función de la experiencia
par(mar=c(5,7,1,1))
bymedian <- with(encuestas, reorder(Experiencia, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")