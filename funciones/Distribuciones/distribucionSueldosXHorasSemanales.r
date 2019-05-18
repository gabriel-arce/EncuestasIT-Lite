# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

# distribución de sueldo netos por hora en función de las horas trabajadas por semana
par(mar=c(5,8,1,1))
bymedian <- with(encuestas, reorder(CargaLaboral, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")