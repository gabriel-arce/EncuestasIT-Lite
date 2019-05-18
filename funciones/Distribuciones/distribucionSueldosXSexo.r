# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

#Distribución de sueldo netos por hora en función del sexo
par(mar=c(5,10,1,1))
bymedian <- with(encuestas, reorder(IdSexo, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")