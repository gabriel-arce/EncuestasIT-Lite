# ----------------------  Importamos funciones  ----------------------  
source("./funciones/main.R")

# todas las (horas trabajadas >= 120 o <= 18) las vamos a considerar anomalas y vamos a desechar esos registros
encuestas <- encuestas[encuestas$horasTrabajadasXSemana < 120, ]
encuestas <- encuestas[encuestas$horasTrabajadasXSemana > 18, ]

title <- "Horas Trabajadas"
xlabel <- "Horas Trabajadas"
ylabel <- "Semanas"

hist(encuestas$horasTrabajadasXSemana, main = title, xlab = xlabel, ylab = ylabel)
rug(encuestas$horasTrabajadasXSemana)

