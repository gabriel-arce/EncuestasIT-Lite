# ----------------------  Importamos funciones  ----------------------  

source("./funciones/quitarSaltosLineaComentarios.R")
source("./funciones/cantidadOcurrenciasStr.R")


# ----------------------  Constantes  ----------------------  

FILE_ENCUESTAS  <- "./datos/encuestas.txt"
FILE_TABLAS  <- "./datos/tablas_anexas.txt"


# ----------------------  Descarga  ----------------------

# Descargamos los datos en crudo del sitio de encuestasIT
if(!file.exists(FILE_ENCUESTAS))
    download.file(  "http://www.encuestasit.com/preguntas-frecuentes/descargar-encuestas", 
                    FILE_ENCUESTAS)

# Descargamos la información de las tablas anexas
if(!file.exists(FILE_TABLAS))
    download.file(  "http://www.encuestasit.com/preguntas-frecuentes/descargar-tablas-anexas", 
                    FILE_TABLAS)


# Ya tenemos los datos de las encuestas en crudos, pero antes de levantarlos a una tabla necesitamos
# hacer algun preprocesamiento, ya que uno de los campos es "observaciones" y en el mismo se incluye
# un texto libre que puede contenter comas, saltos de linea, y demas caracteres que arruinarian el parseo
# al levantar el archivo directamente como un .csv, por lo que necesitamos "sanitizar" este campo antes
# de proseguir

# ----------------------  Preprocesado archivo texto  ----------------------

# Levantamos el archico como un documento de texto
txtdata <- readLines(FILE_ENCUESTAS) 

# Quitamos todas las lineas en blanco, no tienen razon de ser en el archivo.
empty_lines = grepl('^\\s*$', txtdata)
txtdata = txtdata[! empty_lines]

# Quitamos los saltos de linea dentro de los comentarios
txtdata <- quitarSaltosLineaComentarios(txtdata)

# Quitamos los ", ," por ",," 
txtdata <- gsub(", ,", ",,", txtdata)
# y los ",  ," por ",,"
txtdata <- gsub(",  ,", ",,", txtdata)
# y los ", \r\n" por ","
txtdata <- gsub(", \r\n", ",\r\n", txtdata)
# y los ",  \r\n" por ","
txtdata <- gsub(",  \r\n", ",\r\n", txtdata)

# Quitamos los ",False, " y lo reemplazamos por ",False," (3 veces por si hay varios espacios)
txtdata <- gsub(",False, ", ",False,", txtdata)
txtdata <- gsub(",False,  ", ",False,", txtdata)
txtdata <- gsub(",False,   ", ",False,", txtdata)

# Quitamos los ",True, " y lo reemplazamos por ",True," (3 veces por si hay varios espacios)
txtdata <- gsub(",True, ", ",True,", txtdata)
txtdata <- gsub(",True,  ", ",True,", txtdata)
txtdata <- gsub(",True,   ", ",True,", txtdata)

# Quitamos los ", " y lo reemplazamos por " "
txtdata <- gsub(", ", " ", txtdata)

# Veamos cuantas lineas no tienen la cantidad de "," que esperamos que tengan
cantComasEsperadas <- cantidadComas(txtdata[1])
dataCantComas <- lapply(txtdata, cantidadComas)

# Buscamos los renglones que tienen más o menos comas de las esperadas
renglonesInconsistentes <- txtdata[dataCantComas != cantComasEsperadas]
renglonesConsistentes <- txtdata[dataCantComas == cantComasEsperadas]

# Cantidad de renglones inconsistentes: 
length(renglonesInconsistentes)

# Cantidad de renglones consistentes:
length(renglonesConsistentes)

# Cantidad de renglones total:
length(txtdata)

# Grabamos el procesado que hicimos hasta ahora
write(renglonesInconsistentes, "./datos/encuestas_unclear.txt")
write(renglonesConsistentes, "./datos/encuestas_clear.txt")

# TODO: Falta resolver el tema de todos los registros rotos por las comas.
# Quedo a la espera de que Ignacio publique el nuevo dataset con otro delimitador 
# Mientras continuo analisís con los datos que están OK

# ----------------------  Levantar DataFrame  ----------------------

# Levantamos el archivo ya formateado, 
encuestas <- read.csv("./datos/encuestas_clear.txt")

# ----------------------  Tratamiento Valores Faltantes  ----------------------


# Analisamos valores faltantes
na_count <-sapply(encuestas, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
plot(na_count$na_count)

# Quitamos las columnas que tienen todos NA 
encuestas$SalarioActualBruto <- NULL
encuestas$IdArea <- NULL
encuestas$puestoGenerico <- NULL
encuestas$especialidadGenerico <- NULL

# La mayoria de los datos de la columna NivelRemunerativo son NA, y sumado a que tampoco hay
# referencias sobre ese campo, lo mejor parece ser que es quitarla
# antes veamos si existe alguna correlaciòn entre el nivelRemunerativo y el salario
boxplot(SalarioActualNeto ~ NivelRemunerativo, data = encuestas)

# No se ve ninguna relación clara, quitamos entonces nivelRemunerativo
encuestas$NivelRemunerativo <- NULL

# Vemos que hay un registro con IdPais en NA, lo eliminamos
encuestas <- encuestas[complete.cases(encuestas[,"IdPais"]),]

# Nos quedamos solo con los de Argentina, ya que para este estudio no nos interesa el resto. 
# Ademas sus salarios están expresados en moneda local de cada país, lo que requeriria una conversión
# Por el momento el estudio se basará en los datos de Argentina
encuestas <- subset(encuestas, IdPais == 1)
encuestas$IdPais <- NULL


# Esto último tambien eliminó el unico registro que tenia un NA en NivelDeDesconfianza, genial!


# Analisamos valores faltantes nuevamente
na_count <-sapply(encuestas, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
plot(na_count$na_count)

# Parece estar suficientemente limpio el dataset de valores vacios.


# ----------------------  Ajuste de tipos de datos  ----------------------

# Pasamos la fecha al formato correcto
encuestas$Fecha <- as.character(encuestas$Fecha)
encuestas$Fecha <- as.POSIXlt(encuestas$Fecha)


# Levantamos las tablas auxiliares
tabla_sexo <- read.table(FILE_TABLAS, header = TRUE, sep=",", nrows =3, comment.char = "-")
tabla_nivel_educativo <- read.table(FILE_TABLAS, header = TRUE, sep="\t", nrows = 10, skip=7, comment.char = "-")
tabla_tipo_empresa <- read.table(FILE_TABLAS, header = TRUE, sep="\t", nrows = 7, skip=20, comment.char = "-")
tabla_provincia <- read.table(FILE_TABLAS, header = TRUE, sep="\t", nrows = 413, skip=30)
tabla_puesto <- read.table(FILE_TABLAS, header = TRUE, sep="\t", nrows = 95, skip=446)
tabla_tecnologia <- read.table(FILE_TABLAS, header = TRUE, sep="\t", nrows = 72, skip=544)

# Quitamos las provincias que no sean de argentina
tabla_provincia <- tabla_provincia[tabla_provincia$IdPais == 1, ]

# Seteamos los factors
table(encuestas$IdSexo)
encuestas$IdSexo <- factor(encuestas$IdSexo, levels = tabla_sexo$IdSexo, labels = tabla_sexo$Nombre)
table(encuestas$IdSexo)

table(encuestas$IdNivelEducativo)
encuestas$IdNivelEducativo <- factor(encuestas$IdNivelEducativo, levels = tabla_nivel_educativo$IdNivelEducativo, labels = tabla_nivel_educativo$Nombre)
table(encuestas$IdNivelEducativo)

table(encuestas$IdTipoDeEmpresa)
encuestas$IdTipoDeEmpresa <- factor(encuestas$IdTipoDeEmpresa, levels = tabla_tipo_empresa$IdTipoDeEmpresa, labels = tabla_tipo_empresa$Nombre)
table(encuestas$IdTipoDeEmpresa)

table(encuestas$IdProvincia)
encuestas$IdProvincia <- factor(encuestas$IdProvincia, levels = tabla_provincia$IdProvincia, labels = tabla_provincia$Nombre)
table(encuestas$IdProvincia)

table(encuestas$IdPuesto)
encuestas$IdPuesto <- factor(encuestas$IdPuesto, levels = tabla_puesto$IdPuesto, labels = tabla_puesto$Nombre)
table(encuestas$IdPuesto)

table(encuestas$IdTecnologiaPrincipal)
encuestas$IdTecnologiaPrincipal <- factor(encuestas$IdTecnologiaPrincipal, levels = tabla_tecnologia$IdTecnologiaPrincipal, labels = tabla_tecnologia$Nombre)
table(encuestas$IdTecnologiaPrincipal)

table(encuestas$TrabajaDesdeCasa)
encuestas$TrabajaDesdeCasa <- factor(encuestas$TrabajaDesdeCasa, levels = c("True", "False"), labels = c("Si", "No"))
table(encuestas$TrabajaDesdeCasa)

table(encuestas$LeGustaTrabajarDesdeCasa)
encuestas$LeGustaTrabajarDesdeCasa <- factor(encuestas$LeGustaTrabajarDesdeCasa, levels = c("True", "False"), labels = c("Si", "No"))
table(encuestas$LeGustaTrabajarDesdeCasa)

table(encuestas$CambioPorMejorSalario)
encuestas$CambioPorMejorSalario <- factor(encuestas$CambioPorMejorSalario, levels = c("True", "False"), labels = c("Si", "No"))
table(encuestas$CambioPorMejorSalario)

table(encuestas$CambioPorMejorAmbiente)
encuestas$CambioPorMejorAmbiente <- factor(encuestas$CambioPorMejorAmbiente, levels = c("True", "False"), labels = c("Si", "No"))
table(encuestas$CambioPorMejorAmbiente)

table(encuestas$CambioPorFormaDeTrabajo)
encuestas$CambioPorFormaDeTrabajo <- factor(encuestas$CambioPorFormaDeTrabajo, levels = c("True", "False"), labels = c("Si", "No"))
table(encuestas$CambioPorFormaDeTrabajo)

table(encuestas$CambioPorTecnologia)
encuestas$CambioPorTecnologia <- factor(encuestas$CambioPorTecnologia, levels = c("True", "False"), labels = c("Si", "No"))
table(encuestas$CambioPorTecnologia)

table(encuestas$CambioPorCercania)
encuestas$CambioPorCercania <- factor(encuestas$CambioPorCercania, levels = c("", "True", "False"), labels = c("No informa", "Si", "No"))
table(encuestas$CambioPorCercania)

table(encuestas$CambioPorMenorCargaHoraria)
encuestas$CambioPorMenorCargaHoraria <- factor(encuestas$CambioPorMenorCargaHoraria, levels = c("", "True", "False"), labels = c("No informa", "Si", "No"))
table(encuestas$CambioPorMenorCargaHoraria)

table(encuestas$CambioPorOportunidadDeCarrera)
encuestas$CambioPorOportunidadDeCarrera <- factor(encuestas$CambioPorOportunidadDeCarrera, levels = c("", "True", "False"), labels = c("No informa", "Si", "No"))
table(encuestas$CambioPorOportunidadDeCarrera)

table(encuestas$TienePersonasACargo)
encuestas$TienePersonasACargo <- factor(encuestas$TienePersonasACargo, levels = c("", "True", "False"), labels = c("No informa", "Si", "No"))
table(encuestas$TienePersonasACargo)

# ---------------------- Ajuste de salarios por inflación  ---------------------- 

# Vamos a crear 2 nuevas caracteristicas para cada valor monetario,
# Uno es el valor expresado en Dolares a la fecha de la encuesta
# Otro es el valor expresado en Pesos aplicando el indice de inflación a enero del 2017

# Para el valor del dolar en cada fecha se tomará el valor oficial.
# Para el valor ajustado por inflaciòn se tomará la inflación oficial y la inflación congreso


## TODO


# ---------------------- Limpieza de valores anomalos  ---------------------- 

# todas las (edad > 65) las vamos a considerar anomalas y vamos a desechar esos registros
encuestas <- encuestas[encuestas$Edad < 66, ]
hist(encuestas$Edad)
rug(encuestas$Edad)

# todas las (horas trabajadas >= 120 o <= 18) las vamos a considerar anomalas y vamos a desechar esos registros
encuestas <- encuestas[encuestas$horasTrabajadasXSemana < 120, ]
encuestas <- encuestas[encuestas$horasTrabajadasXSemana > 18, ]
hist(encuestas$horasTrabajadasXSemana)
rug(encuestas$horasTrabajadasXSemana)

# todos los (meses en el puesto actual >= 360) los vamos a considerar anomalas y vamos a desechar esos registros
encuestas <- encuestas[encuestas$MesesEnElPuestoActual < 480, ]
hist(encuestas$MesesEnElPuestoActual)
rug(encuestas$MesesEnElPuestoActual)

# todos los (salario actual neto >= 150000) los vamos a considerar anomalas y vamos a desechar esos registros
encuestas <- encuestas[encuestas$SalarioActualNeto < 150000, ]
hist(encuestas$SalarioActualNeto)
rug(encuestas$SalarioActualNeto)

# todos los (salario ideal neto >= 150000) los vamos a considerar anomalas y vamos a desechar esos registros
encuestas <- encuestas[encuestas$SalarioIdealNeto < 150000, ]
hist(encuestas$SalarioIdealNeto)
rug(encuestas$SalarioIdealNeto)

# todos los (Edad < 17 y Edad > 0) los vamos a considerar anomalas y vamos a desechar esos registros
encuestas <- encuestas[encuestas$Edad == 0 | encuestas$Edad > 17, ]
hist(encuestas$Edad)
rug(encuestas$Edad)

# Borramos los levels que no se usan
encuestas <- droplevels(encuestas)


# ---------------------- Creación de caracteristicas  ---------------------- 

encuestas$DiferenciaSalarioRealIdeal <- encuestas$SalarioIdealNeto - encuestas$SalarioActualNeto

encuestas$SalarioNetoPorHora <- encuestas$SalarioActualNeto / encuestas$horasTrabajadasXSemana

# Carga laboral: En relación a las horas que dedica al trabajo
encuestas$CargaLaboral <- cut(encuestas$horasTrabajadasXSemana, 5, labels = c("Part Time", "Full Time", "Extra Time", "Very Extra Time", "Extreme Time"))

# Antiguedad: Nivel de antiguedad en el actual trabajo
encuestas$Antiguedad <- cut(encuestas$MesesEnElPuestoActual,quantile(encuestas$MesesEnElPuestoActual,(0:4)/4), labels = c("Junior", "SemiSenior", "Senior", "Expert"))

# Experiencia: Nivel de antiguedad en cualquier trabajo
encuestas$Experiencia <- cut(encuestas$MesesDeExperiencia,quantile(encuestas$MesesDeExperiencia,(0:4)/4), labels = c("Junior", "SemiSenior", "Senior", "Expert"))

# RangoEdad: Rango de edad en el que se ubica
encuestas$RangoEdad <- cut(encuestas$Edad, c(0, 1, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65), labels = c("No Informa", "20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50-55", "55-60", "60-65"))

# ---------------------- Limpieza de valores anomalos en caracteristicas creadas  ---------------------- 

# Todas las diferencias de salario real e ideal mayores a 25000 y menores a -5000 las eliminamos
# Por considerarlas anomalas.
encuestas <- encuestas[encuestas$DiferenciaSalarioRealIdeal < 25000, ]
encuestas <- encuestas[encuestas$DiferenciaSalarioRealIdeal > -5000, ]
hist(encuestas$DiferenciaSalarioRealIdeal)
rug(encuestas$DiferenciaSalarioRealIdeal)

# Todos los salarios netos por hora mayores a 1500 los vamos a eliminar por considerarlos anomalos
encuestas <- encuestas[encuestas$SalarioNetoPorHora < 1500, ]
hist(encuestas$SalarioNetoPorHora)
rug(encuestas$SalarioNetoPorHora)

# Todos los "Extreme Time" los vamos a borrar por no ser representativos
encuestas <- encuestas[encuestas$CargaLaboral != "Extreme Time", ]
table(encuestas$CargaLaboral)

# Borramos los levels que no se usan
encuestas <- droplevels(encuestas)

# ---------------------- Exploración grafica ----------------------

# distribución de sueldo netos por hora en función del puesto
par(mar=c(5,18,1,1))
bymedian <- with(encuestas, reorder(IdPuesto, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")

# distribución de sueldo netos por hora en función del nivel educativo
par(mar=c(5,15,1,1))
bymedian <- with(encuestas, reorder(IdNivelEducativo, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")

# distribución de sueldo netos por hora en función del sexo
par(mar=c(5,10,1,1))
bymedian <- with(encuestas, reorder(IdSexo, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")

# distribución de sueldo netos por hora en función de las horas trabajadas por semana
par(mar=c(5,8,1,1))
bymedian <- with(encuestas, reorder(CargaLaboral, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")

# distribución de sueldo netos por hora en función de la experiencia
par(mar=c(5,7,1,1))
bymedian <- with(encuestas, reorder(Experiencia, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")


# distribución de sueldo netos por hora en función de la antiguedad
par(mar=c(5,7,1,1))
bymedian <- with(encuestas, reorder(Antiguedad, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")


# distribución de sueldo netos por hora en función de la tecnologia
par(mar=c(5,7,1,1))
bymedian <- with(encuestas, reorder(IdTecnologiaPrincipal, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")


# distribución de sueldo netos por hora en función de la Edad
par(mar=c(5,7,1,1))
bymedian <- with(encuestas, reorder(RangoEdad, -SalarioNetoPorHora, median))
boxplot(SalarioNetoPorHora ~ bymedian, encuestas, horizontal = TRUE, las = 2, col="green")


# Veamos la relación entre el salario y la antiguedad, para puestos bajos y medios
with(subset(encuestas, SalarioActualNeto < 40000), plot(SalarioActualNeto, MesesEnElPuestoActual) )


# Veamos la relación entre el salario y la edad, para el año 2013, discriminado por sexo
par(mar=c(5,5,1,1))
with(subset(encuestas, Fecha$year == 113 & Edad > 1 & IdSexo == "Masculino"), plot(SalarioNetoPorHora, Edad))
with(subset(encuestas, Fecha$year == 113 & Edad > 1 & IdSexo == "Femenino"), points(SalarioNetoPorHora, Edad, col="red"))
model <- lm(Edad ~ SalarioNetoPorHora, subset(encuestas, Fecha$year == 113 & Edad > 1 & IdSexo == "Masculino"))
abline(model, lwd = 2)
model <- lm(Edad ~ SalarioNetoPorHora, subset(encuestas, Fecha$year == 113 & Edad > 1 & IdSexo == "Femenino"))
abline(model, lwd = 2, col="red")
legend("topright", legend = c("Masculino", "Femenino"), pch = 1, col = c("black", "red"))



