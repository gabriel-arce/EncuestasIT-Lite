# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

# Veamos la relación entre el salario y la antiguedad, para puestos bajos y medios
with(subset(encuestas, SalarioActualNeto < 40000), plot(SalarioActualNeto, MesesEnElPuestoActual) )