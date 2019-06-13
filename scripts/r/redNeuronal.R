
source("./funciones/main.R")

# --- Include de Librerias --- 
#install.packages("knitr")
#install.packages("lattice")
#install.packages("ggplot2")
#install.packages("caret")
#install.packages("nnet")
#install.packages("neuralnet")
#install.packages("devtools")
#install.packages("rmarkdown")

#library(knitr)
#library(lattice)
#library(ggplot2)
#library(caret)
#library(nnet)
#library(neuralnet)
#library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

#Instalar y cargar packages
requiredPackages = c("knitr", "lattice", "ggplot2", "caret", "nnet", "neuralnet", "devtools", "rmarkdown");

installAndLoad(requiredPackages)





knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE, 
                      cache = TRUE)

### Carga de los datos  
# ----------------------  Levantar DataFrame  ----------------------

# Levantamos el archivo ya formateado,
#encuestas <- read.csv("./datos.csv")
encuestas$X <- NULL

### Resumen de datos
#Visualizamos un resumen de los datos que vamos a usar para esta clasificación.
# ----------------------  imprimir resumen de campos  ----------------------
str(encuestas)

summary(encuestas)

# ----------------------   primeros campos  ---------------------- 
kable(head(encuestas))

### Preprocesado de datos  

#- Borramos caracteristicas que estan demasiado vinculadas a lo que vamos a predecir  
#- Separamos la columna a predecir (SalarioNetoPorHora)   
#- Todos las columnas de tipo "Factor" (Enumeración) las llevamos a int (entero)  
#- Los valores faltantes (NA) los ponemos en 0  
#- Se escalan los valores entre 0 y 1
#- Se quitan algunas caracteristicas poco imporantes que no se tendrán en cuenta en el modelo


# Borramos caracteristicas que estan demasiado vinculadas a lo que vamos a predecir
# No las vamos a utilizar ya que queremos poder predecir el salario en base a
# otras caracteristicas del empleo
encuestas$SalarioActualNeto <- NULL
encuestas$SalarioIdealNeto <- NULL
encuestas$DiferenciaSalarioRealIdeal <- NULL
encuestas$RangoSalario <- NULL

# Convercion de Enums a Int
encuestas$IdSexo <- as.numeric(encuestas$IdSexo)
encuestas$IdNivelEducativo <- as.numeric(encuestas$IdNivelEducativo)
encuestas$IdTipoDeEmpresa <- as.numeric(encuestas$IdTipoDeEmpresa)
encuestas$IdProvincia <- as.numeric(encuestas$IdProvincia)
encuestas$IdPuesto <- as.numeric(encuestas$IdPuesto)
encuestas$TrabajaDesdeCasa <- as.numeric(encuestas$TrabajaDesdeCasa)
encuestas$LeGustaTrabajarDesdeCasa <- as.numeric(encuestas$LeGustaTrabajarDesdeCasa)
encuestas$CambioPorMejorSalario <- as.numeric(encuestas$CambioPorMejorSalario)
encuestas$CambioPorMejorAmbiente <- as.numeric(encuestas$CambioPorMejorAmbiente)
encuestas$CambioPorFormaDeTrabajo <- as.numeric(encuestas$CambioPorFormaDeTrabajo)
encuestas$CambioPorTecnologia <- as.numeric(encuestas$CambioPorTecnologia)
encuestas$NoCambio <- as.numeric(encuestas$NoCambio)
encuestas$NivelDeDesconfianza <- as.numeric(encuestas$NivelDeDesconfianza)
encuestas$CambioPorCercania <- as.numeric(encuestas$CambioPorCercania)
encuestas$CambioPorMenorCargaHoraria <- as.numeric(encuestas$CambioPorMenorCargaHoraria)
encuestas$CambioPorOportunidadDeCarrera <- as.numeric(encuestas$CambioPorOportunidadDeCarrera)
encuestas$TienePersonasACargo <- as.numeric(encuestas$TienePersonasACargo)
encuestas$Relaci.nLaboral <- as.numeric(encuestas$Relaci.nLaboral)
encuestas$RangoHora <- as.numeric(encuestas$RangoHora)
encuestas$CargaLaboral <- as.numeric(encuestas$CargaLaboral)
encuestas$Antiguedad <- as.numeric(encuestas$Antiguedad)
encuestas$Experiencia <- as.numeric(encuestas$Experiencia)
encuestas$RangoEdad <- as.numeric(encuestas$RangoEdad)
encuestas$IdTecnologiaPrincipal <- as.numeric(encuestas$IdTecnologiaPrincipal)
encuestas$CargaLaboral <- as.numeric(encuestas$CargaLaboral)
encuestas$Semestre <- as.numeric(encuestas$Semestre)

# Los valores faltantes (NA) los ponemos en 0
encuestas[is.na(encuestas$CantidadDeMesesParaCambiarDeTrabajo),]$CantidadDeMesesParaCambiarDeTrabajo <- 0
encuestas[is.na(encuestas$Relaci.nLaboral),]$Relaci.nLaboral <- 0
encuestas[is.na(encuestas$RangoHora),]$RangoHora <- 0
encuestas[is.na(encuestas$Experiencia),]$Experiencia <- 0
encuestas[is.na(encuestas$Antiguedad),]$Antiguedad <- 0

# Guardamos el max y min salario neto por hora, para poder reescalar el valor
maxSal = max(encuestas$SalarioNetoPorHora)
minSal = min(encuestas$SalarioNetoPorHora)

# Escalado entre 0 y 1
range01 <- function(x){
  time1 <- as.numeric(x-min(x))
  time2 <- as.numeric(max(x)-min(x))
  time1 / time2
  
}

sumario <- as.data.frame(summary(encuestas))

factorizando <- lapply(encuestas[sapply(encuestas, is.numeric)], range01)

encuestas <- as.data.frame(factorizando) 

# Output
Output <- encuestas$SalarioNetoPorHora
encuestas$SalarioNetoPorHora <- NULL

# Borramos algunas caracteristicas a fin de simplificar el modelo
encuestas$LeGustaTrabajarDesdeCasa <- NULL
encuestas$CambioPorMejorAmbiente <- NULL
encuestas$CambioPorFormaDeTrabajo <- NULL
encuestas$CambioPorTecnologia <- NULL
encuestas$NoCambio <- NULL
encuestas$NivelDeDesconfianza <- NULL
encuestas$CambioPorCercania <- NULL
encuestas$CambioPorMenorCargaHoraria <- NULL
encuestas$CambioPorOportunidadDeCarrera <- NULL
encuestas$Relaci.nLaboral <- NULL
encuestas$RangoHora <- NULL
encuestas$Semestre <- NULL


### Separación en sets

#Selección de una submuestra del 70% para entrenamiento y 30% para test.

set.seed(101)
indices <- sample(1:nrow(encuestas),size=round(0.3*nrow(encuestas)))

entrenamiento_input <- encuestas[-indices,]
entrenamiento_output = Output[-indices]
entrenamiento <- entrenamiento_input
entrenamiento$Output <- entrenamiento_output

test_input <- encuestas[indices,]
test_output <- Output[indices]


### Armado de la red neuronal

# Outputs:
nombres <- names(encuestas)

# Inputs:
f <- paste(nombres,collapse=' + ')
f <- paste('Output ~',f)

# Formula (Output en función de los inputs):
f <- as.formula(f)

# -------------------
# Creación y entrenamiento de la red neuronal

nn <- neuralnet(formula = f,                
                data = entrenamiento,
                threshold = 0.1,
                hidden=c(12,8),                
                linear.output=TRUE,
                stepmax = 20000)


### Evaluación del modelo

# Calculamos las predicciones usando el modelo para el conjunto de TEST
predicciones_test <- compute(nn,test_input)

# Reescalamos los resultados (que estaban escalados al rango [0;1]) para que sean entendibles
salariosPredichos_test <- predicciones_test$net.result * (maxSal - minSal) + minSal
salariosReales_test <- test_output * (maxSal - minSal) + minSal

# Calculamos la diferencia entre el valor predicho y el valor real, en valor absoluto
errorPrediccion_test <- salariosReales_test - salariosPredichos_test
errorPrediccionAbs_test <- abs(errorPrediccion_test)

# Calculamos la media del error
errorPromedio_test <- mean(errorPrediccionAbs_test)
salarioPromedioReal_test <- mean(salariosReales_test)
salarioPromedioPredicho_test <- mean(salariosReales_test)


hist(errorPrediccion_test, breaks = 300, main="Histograma del error", xlim=c(-500, 500))

# Armamos resumen con los primeros casos
resumen <- test_input
resumen$SalarioNetoPorHoraReal <- salariosReales_test
resumen$SalarioNetoPorHoraPredicho <- salariosPredichos_test
resumen$ErrorPrediccion <- errorPrediccion_test

# Imprimimos resultado de implementar el modelo a los casos de test. 
# Para facilitar la lectura redondeamos en 2 decimales.
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
kable(as.data.frame(t(head(round_df(resumen, 2), 10))))

# Calculamos las predicciones usando el modelo para el conjunto de ENTRENAMIENTO
predicciones_entrenamiento <- compute(nn,entrenamiento_input)

# Reescalamos los resultados (que estaban escalados al rango [0;1]) para que sean entendibles
salariosPredichos_entrenamiento <- predicciones_entrenamiento$net.result * (maxSal - minSal) + minSal
salariosReales_entrenamiento <- entrenamiento_output * (maxSal - minSal) + minSal

# Calculamos la diferencia entre el valor predicho y el valor real, en valor absoluto
errorPrediccion_entrenamiento <- abs(salariosReales_entrenamiento - salariosPredichos_entrenamiento)

# Calculamos la media del error
errorPromedio_entrenamiento <- mean(errorPrediccion_entrenamiento)
salarioPromedioReal_test <- mean(salariosReales_entrenamiento)
salarioPromedioPredicho_test <- mean(salariosReales_entrenamiento)

### Visualización del modelo
plot(nn)
