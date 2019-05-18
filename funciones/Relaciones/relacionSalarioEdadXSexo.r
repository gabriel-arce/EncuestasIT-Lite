# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

# Veamos la relación entre el salario y la edad, para el año actual, discriminado por sexo
par(mar=c(5,5,1,1))
with(subset(encuestas, Fecha$year == 113 & Edad > 1 & IdSexo == "Masculino"), plot(SalarioNetoPorHora, Edad))
with(subset(encuestas, Fecha$year == 113 & Edad > 1 & IdSexo == "Femenino"), points(SalarioNetoPorHora, Edad, col="red"))
model <- lm(Edad ~ SalarioNetoPorHora, subset(encuestas, Fecha$year == 113 & Edad > 1 & IdSexo == "Masculino"))
abline(model, lwd = 2)
model <- lm(Edad ~ SalarioNetoPorHora, subset(encuestas, Fecha$year == 113 & Edad > 1 & IdSexo == "Femenino"))
abline(model, lwd = 2, col="red")
legend("topright", legend = c("Masculino", "Femenino"), pch = 1, col = c("black", "red"))