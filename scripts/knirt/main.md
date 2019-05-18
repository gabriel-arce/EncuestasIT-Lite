EncuestasIT
================

Encuestas IT
------------

Este es un analisís general de los datos de las encuestas recolectadas por el sitio <http://www.encuestasit.com/> Los datos en crudo de las encuestas están disponibles en el sitio, en la sección de Preguntas Frecuentes. Todo el trabajo fue realizado con la autorización de los creadores del sitio [encuestasIt](http://www.encuestasit.com/)

### Los objetivos de este trabajo son puramente academicos e incluyen:

-   Utilizar tecnicas de exploración de datos para encontrar relaciones de interes entre las variables.
-   Fomentar el uso de herramientas de analisís de datos.
-   Demostrar la capacidad de R para la exploración y analisis de datos y para elaborar informes claros.

Indice
------

TODO

Obtención de datos
------------------

URL datos en crudo de las encuestas: <http://www.encuestasit.com/preguntas-frecuentes/descargar-encuestas>
URL datos de tabulado de tablas auxiliares: <http://www.encuestasit.com/preguntas-frecuentes/descargar-tablas-anexas>

Estos archivos se guardarán localmente en ../../datos/encuestas.txt y ../../datos/tablas\_anexas.txt respectivamente.

``` r
# Descargamos los datos en crudo del sitio de encuestasIT
if (!file.exists(FILE_ENCUESTAS))
    download.file(URL_ENCUESTAS, FILE_ENCUESTAS)

# Descargamos la información de las tablas anexas
if (!file.exists(FILE_TABLAS))
    download.file(URL_TABLAS, FILE_TABLAS)
```

Preprocesado de datos
---------------------

Los datos están en formato CSV. Se espera es que cada linea sea un regitro y cada atributo esté separado por una ",".

Hasta la versión publicada al 23/02/2017 ocurre un problema grave en el formato del documento, ya que uno de los campos (*Observaciones*) es de texto libre y no se encuentra "sanitizado" por lo que dentro del mismo puden haber "," y saltos de linea, lo que arruina el formato del documento. Algo similiar ocurre con el campo *Medio para buscar trabajo* que tiene valores separados por ","

> El 22/02/2017 se solicitó al responsable del sitio que la proxima exportación de información se haga usando otro delimitador que no sea "," para evitar estos problemas

Antes de levantar los datos en una tabla necesitamos preprocesar el documento para solucionar estos problemas.

``` r
# Levantamos el archico como un documento de texto
txtdata <- readLines(FILE_ENCUESTAS)

# Quitamos todas las lineas en blanco, no tienen razon de ser en el archivo.
empty_lines = grepl('^\\s*$', txtdata)
txtdata = txtdata[!empty_lines]

# Quitamos los saltos de linea dentro de los comentarios
txtdata <- quitarSaltosLineaComentarios(txtdata)

# Quitamos los ", ," por ",,"
txtdata <- gsub(", ,", ",,", txtdata)
# y los ",  ," por ",,"
txtdata <- gsub(",  ,", ",,", txtdata)

# quitamos los espacios al final de cada linea (trim end)
txtdata <- gsub("\\s+$", "", txtdata)

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

# Quitamos los ", " y lo reemplazamos por " "
txtdata <- gsub(", ", " ", txtdata)

# Veamos cuantas lineas no tienen la cantidad de "," que esperamos que tengan
cantComasEsperadas <- cantidadComas(txtdata[1])
dataCantComas <- lapply(txtdata, cantidadComas)

# Buscamos los renglones que tienen más o menos comas de las esperadas
renglonesInconsistentes <-
    txtdata[dataCantComas != cantComasEsperadas]
renglonesConsistentes <-
    txtdata[dataCantComas == cantComasEsperadas]
```

Cantidad de renglones total:26682
Cantidad de renglones inconsistentes:267
Cantidad de renglones consistentes:26415

Vemos que existen algunos registros que no se pueden formatear bien. Por ser una cantidad poco significativa no le vamos a dar tratamiento a esos registros por el momento.

> TODO:Darle tratamiento a los registros inconsistentes para dejarlos consistentes. Queda como mejora revisar estos registros para darle el correcto formato.

``` r
# Grabamos el procesado que hicimos hasta ahora
write(renglonesInconsistentes,
      "../../datos/encuestas_unclear.txt")
write(renglonesConsistentes, "../../datos/encuestas_clear.txt")
```

Carga de datos en memoria
-------------------------

Levantamos los datos en un DataFrame con el nombre \* encuestas * De aquí en más la variable * encuestas \* será nuestra set de datos principal

``` r
# ----------------------  Levantar DataFrame  ----------------------

# Levantamos el archivo ya formateado,
encuestas <- read.csv("../../datos/encuestas_clear.txt", quote = "")
```

Tratamiento de valores faltantes
--------------------------------

Antes de seguir debemos analisar los valores faltantes (NA) en nuestra tabla

``` r
# ----------------------  Tratamiento Valores Faltantes  ----------------------

# Analisamos valores faltantes
na_count <- sapply(encuestas, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count <- subset(na_count, na_count > 0)
kable(na_count, caption="Cantidad de valores faltantes por columna")
```

|                                     |  na\_count|
|-------------------------------------|----------:|
| CantidadDeMesesParaCambiarDeTrabajo |       8465|
| MedioParaBuscarTrabajo              |          4|
| CantidadDeEmpleadosEnLaOrganizacion |       6203|
| RelaciónLaboral                     |       7587|
| NivelRemunerativo                   |      14136|
| SalarioActualBruto                  |      22081|
| IdArea                              |      22083|

``` r
# Quitamos las columnas que tienen todos NA 
encuestas$SalarioActualBruto <- NULL
encuestas$IdArea <- NULL

# La columna "MedioParaBuscarTrabajo" no va a formar parte del analisis, pues no interesa en principio.
encuestas$MedioParaBuscarTrabajo <- NULL

# La mayoria de los datos de la columna NivelRemunerativo son NA, y sumado a que tampoco hay
# referencias sobre ese campo, lo mejor parece ser que es quitarlo
encuestas$NivelRemunerativo <- NULL


# Analisamos valores faltantes nuevamente
na_count <- sapply(encuestas, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count <- subset(na_count, na_count > 0)
kable(na_count, caption="Cantidad de valores faltantes por columna")
```

|                                     |  na\_count|
|-------------------------------------|----------:|
| CantidadDeMesesParaCambiarDeTrabajo |       8465|
| CantidadDeEmpleadosEnLaOrganizacion |       6203|
| RelaciónLaboral                     |       7587|

Filtrado de datos por ubicación: Solo incluimos Argentina.
----------------------------------------------------------

Restringimos el dominio del analisís a Argentina. Para este analisís nos centraremos en los datos de un solo país.

> Eventualmente esto puede cambiar. En una primera versión del analisís nos limitaremos a Argentina. Se puede cambiar esto para que el analisís sea con datos de otro país, pero hay fenomenos monetarios que son propios de cada nación (inflación, tipo de cambio) que deben ser analisados en detalle en cada caso

``` r
# Nos quedamos solo con los de Argentina, ya que para este estudio no nos interesa el resto. 
# Ademas sus salarios están expresados en moneda local de cada país, lo que requeriria una conversión
# Por el momento el estudio se basará en los datos de Argentina
encuestas <- subset(encuestas, IdPais == 1)
encuestas$IdPais <- NULL
```

Filtrado de datos por año: Solo incluimos 2016
----------------------------------------------

El analisis será solo por un año en especifico.

> El año puede cambiar para obtener el mismo analisís para otros años

``` r
# encuestas2016 <- subset(encuestas, as.POSIXlt(Fecha)$year + 1900 == 2016 )
```

Ajuste de tipos de dato
-----------------------

Antes de seguir debemos ajustar algunos tipos de datos. Las fechas las vamos a trabajar como POSIXlt. Los valores tabulados vamos a setearlos como Factors usando las tablas auxiliares.

``` r
# ----------------------  Ajuste de tipos de datos  ----------------------

# Pasamos la fecha al formato correcto
encuestas$Fecha <- as.character(encuestas$Fecha)
encuestas$Fecha <- as.POSIXlt(encuestas$Fecha)


# Levantamos las tablas auxiliares
tabla_sexo <-
    read.table(
        FILE_TABLAS,
        header = TRUE,
        sep = ",",
        nrows = 3,
        comment.char = "-"
    )
tabla_nivel_educativo <-
    read.table(
        FILE_TABLAS,
        header = TRUE,
        sep = "\t",
        nrows = 10,
        skip = 7,
        comment.char = "-"
    )
tabla_tipo_empresa <-
    read.table(
        FILE_TABLAS,
        header = TRUE,
        sep = "\t",
        nrows = 7,
        skip = 20,
        comment.char = "-"
    )
tabla_provincia <-
    read.table(
        FILE_TABLAS,
        header = TRUE,
        sep = "\t",
        nrows = 413,
        skip = 30
    )
tabla_puesto <-
    read.table(
        FILE_TABLAS,
        header = TRUE,
        sep = "\t",
        nrows = 95,
        skip = 446
    )
tabla_tecnologia <-
    read.table(
        FILE_TABLAS,
        header = TRUE,
        sep = "\t",
        nrows = 72,
        skip = 544
    )


# Quitamos las provincias que no sean de argentina
tabla_provincia <- tabla_provincia[tabla_provincia$IdPais == 1,]


# Seteamos los factors
encuestas$SeSienteMotivado <- factor(encuestas$SeSienteMotivado)
encuestas$SeLoReconoceComoDebiera <-
    factor(encuestas$SeLoReconoceComoDebiera)
encuestas$SeSientePresionado <- factor(encuestas$SeSientePresionado)
encuestas$SeSienteSobreexigido <-
    factor(encuestas$SeSienteSobreexigido)

encuestas$IdSexo <-
    factor(encuestas$IdSexo,
           levels = tabla_sexo$IdSexo,
           labels = tabla_sexo$Nombre)
kable(as.data.frame(table(encuestas$IdSexo)), caption = "Cantidad de registros por sexo")
```

| Var1       |   Freq|
|:-----------|------:|
| No informa |   1626|
| Masculino  |  17327|
| Femenino   |   2285|

``` r
encuestas$IdNivelEducativo <-
    factor(
        encuestas$IdNivelEducativo,
        levels = tabla_nivel_educativo$IdNivelEducativo,
        labels = tabla_nivel_educativo$Nombre
    )
kable(as.data.frame(table(encuestas$IdNivelEducativo)), caption = "Cantidad de registros por nivel educativo")
```

| Var1                                     |  Freq|
|:-----------------------------------------|-----:|
| No informa                               |   379|
| Primario                                 |    37|
| Secundario en curso o incompleto         |   211|
| Secundario completo                      |  1315|
| Terciario en curso o incompleto          |  1644|
| Terciario completo                       |  3816|
| Universitario en curso o incompleto      |  7195|
| Universitario completo                   |  4874|
| Master o postgrado en curso o incompleto |   977|
| Master o postgrado completo              |   790|

``` r
encuestas$IdTipoDeEmpresa <-
    factor(
        encuestas$IdTipoDeEmpresa,
        levels = tabla_tipo_empresa$IdTipoDeEmpresa,
        labels = tabla_tipo_empresa$Nombre
    )
kable(as.data.frame(table(encuestas$IdTipoDeEmpresa)), caption = "Cantidad de registros por tipo de empresa")
```

| Var1                          |   Freq|
|:------------------------------|------:|
| No informa                    |   1765|
| Una empresa privada           |  17028|
| Un organismo estatal          |   1189|
| Una ONG                       |     76|
| Otro                          |    526|
| Mi propia empresa             |    524|
| Soy independiente / freelance |    130|

``` r
encuestas$IdProvincia <-
    factor(encuestas$IdProvincia,
           levels = tabla_provincia$IdProvincia,
           labels = tabla_provincia$Nombre)
kable(as.data.frame(table(encuestas$IdProvincia)), caption = "Cantidad de registros por provincia")
```

| Var1                |  Freq|
|:--------------------|-----:|
| Capital Federal     |  7808|
| GBA Zona Norte      |   498|
| GBA Zona Oeste      |   121|
| GBA Zona Sur        |   107|
| Costa AtlÃ¡ntica    |    27|
| Buenos Aires        |  8072|
| Catamarca           |    17|
| CÃ³rdoba            |  1965|
| Chaco               |   105|
| Chubut              |    51|
| Corrientes          |   117|
| Entre RÃ­os         |   142|
| Formosa             |    22|
| Jujuy               |    32|
| La Pampa            |    18|
| La Rioja            |     8|
| Misiones            |   128|
| Mendoza             |   383|
| NeuquÃ©n            |   175|
| RÃ­o Negro          |    92|
| Salta               |    50|
| Santa Cruz          |    22|
| Santiago del Estero |    19|
| Sante Fe            |   882|
| San Juan            |    60|
| San Luis            |    79|
| Tierra del Fuego    |    13|
| TucumÃ¡n            |   225|

``` r
encuestas$IdPuesto <-
    factor(encuestas$IdPuesto,
           levels = tabla_puesto$IdPuesto,
           labels = tabla_puesto$Nombre)
kable(as.data.frame(table(encuestas$IdPuesto)), caption = "Cantidad de registros por puesto")
```

| Var1                                      |  Freq|
|:------------------------------------------|-----:|
| Desarrollador de software / Programador   |  5564|
| Arquitecto                                |   455|
| Lider de Proyecto                         |  1363|
| Analista Funcional                        |  1487|
| Selector de personal TI                   |   113|
| DiseÃ±ador Web                            |   218|
| InvestigaciÃ³n                            |    50|
| Consultor TI                              |  1015|
| Administrador de Base de Datos            |   373|
| Administrador de Redes                    |   350|
| Analista de GarantÃ­a de Calidad (QA)     |   375|
| Analista de Seguridad InformÃ¡tica        |   201|
| Auditor                                   |    87|
| Comercial de TI                           |   150|
| Director de Sistemas                      |   216|
| Documentador de Sistemas                  |     4|
| Gerente de Proyectos                      |   634|
| Implementador de Sistemas                 |   269|
| Instructor                                |    26|
| Gerente de Compras TI                     |     9|
| Gerente de Comunicaciones                 |    43|
| Gerente de Desarrollo                     |   334|
| Gerente de Operaciones                    |   169|
| Gerente de Seguridad InformÃ¡tica         |    79|
| Gerente de Sistemas                       |   688|
| Gerente de TecnologÃ­a                    |   377|
| Gerente de Ventas TI                      |    53|
| Pasante                                   |    73|
| Mesa de Ayuda (Help Desk)                 |   189|
| Soporte TÃ©cnico                          |   897|
| Pasante (Trainee)                         |   103|
| Otro                                      |   560|
| Administrador de Almacenamiento (Storage) |   757|
| Administrador Middleware                  |    20|
| Administrador de Sistemas                 |   542|
| Administrador de Ambientes                |   707|
| Administrador de Servidores               |   580|
| Coordinador de Grupo                      |   182|
| Lider TÃ©cnico                            |   536|
| Comprador TI                              |    16|
| Data Entry                                |    24|
| Soporte de AplicaciÃ³n                    |   124|
| Soporte de Posventa                       |    19|
| Jefe de Mesa de Ayuda                     |    44|
| Operador NOC                              |    60|
| LÃ­der de Mesa de Ayuda                   |    24|
| LÃ­der de Operaciones                     |    55|
| LÃ­der de GarantÃ­a de Calidad (QA)       |   110|
| LÃ­der de Seguridad InformÃ¡tica          |    51|
| LÃ­der de Soporte TÃ©cnico                |   117|
| Operador de AS400                         |     9|
| Preventa                                  |    38|
| Ejecutivo de Cuenta                       |    54|
| Gerente de Producto                       |    73|
| Analista BI                               |    62|
| Scrum Master                              |    15|
| Auditor de TI                             |    21|
| Gerente General                           |    22|
| Director General                          |    21|
| Administrador de Backup                   |     1|
| Maquetador                                |     7|
| Administrador de Proyectos                |    34|
| Operador de Sistemas                      |     9|
| Tester Funcional                          |    35|
| Analista de Testeo                        |    37|
| Jefe de Compras TI                        |     1|
| Jefe de Comunicaciones                    |     9|
| Jefe de Desarrollo                        |    60|
| Jefe de Operaciones                       |    10|
| Jefe de Seguridad InformÃ¡tica            |    10|
| Jefe de Sistemas                          |    81|
| Jefe de TecnologÃ­a                       |    26|
| Jefe de Ventas TI                         |     1|
| Jefe de GestiÃ³n de Demanda               |     4|
| DiseÃ±ador Multimedia                     |     1|
| Gestor de Cambios de IT                   |     0|
| Gestor de TransiciÃ³n                     |     0|
| Gestor de Configuraciones                 |     0|
| Analista de Cambios                       |     1|
| Gerente de Procesos de IT                 |     1|
| Gestor de Incidentes                      |     1|
| Gestor de Problemas                       |     0|
| Gerente de Demanda                        |     2|
| Gerente de Infraestructura                |     9|
| Jefe de Infraestructura                   |    13|
| Supervisor de Infraestructura             |    13|
| Responsable de Infraestructura            |    16|
| Analista de GestiÃ³n de la Demanda        |     3|
| Responsable de GestiÃ³n de la Demanda     |     2|
| Analista de Procesos                      |     6|
| Responsable de Procesos                   |     3|
| Gestor de Servicios                       |     1|
| Desarrollador BI                          |    25|
| Lider BI                                  |     5|
| Gerente Regional de IT                    |     4|

``` r
encuestas$IdTecnologiaPrincipal <-
    factor(
        encuestas$IdTecnologiaPrincipal,
        levels = tabla_tecnologia$IdTecnologiaPrincipal,
        labels = tabla_tecnologia$Nombre
    )
kable(as.data.frame(table(encuestas$IdTecnologiaPrincipal)), caption = "Cantidad de registros por tecnología principal")
```

| Var1            |  Freq|
|:----------------|-----:|
| .Net            |  4835|
| Java            |  2802|
| PHP             |  1248|
| C++             |   229|
| C               |   109|
| Ruby            |    70|
| Perl            |    33|
| Prolog          |     0|
| Smalltalk       |    78|
| Python          |   128|
| Delphi          |   156|
| VisualBasic     |   299|
| ActionScript    |    49|
| Javascsript     |   211|
| Asp             |    74|
| Html            |   119|
| Haskell         |     1|
| MSSQLServer     |   626|
| Oracle          |  1224|
| PostgreSql      |    39|
| DatawareHousing |   164|
| Cobol           |   230|
| DB2             |    25|
| SAP             |  1192|
| Siebel          |   104|
| Otro            |  1672|
| Windows         |  2973|
| Linux           |   613|
| Solaris         |    37|
| Aix             |    59|
| Hp-ux           |    27|
| Mainframe       |   133|
| AS/400          |   168|
| Assembler       |     6|
| CMS             |    15|
| Cobol           |     0|
| Siebel          |     0|
| ERP             |   218|
| Flash           |     7|
| Flex            |    22|
| Meta4           |    12|
| RPG             |    15|
| Hp              |    72|
| Unix            |   205|
| Clipper         |    12|
| Genexus         |   123|
| MySql Server    |    83|
| Visual FoxPro   |    84|
| Android         |   100|
| Sharepoint      |    80|
| Clarion         |    30|
| QlikView        |    46|
| Microstrategy   |    48|
| BusinessObject  |    18|
| Cognos          |    14|
| Hyperion        |     3|
| Tibco           |     5|
| Board           |     0|
| Pentaho         |     9|
| CISCO           |    90|
| SSIS            |     7|
| VMWare          |    90|
| PowerBuilder    |    21|
| CRM             |    25|
| Objective-C     |    24|
| JD Edwards      |     7|
| PeopleSoft      |    11|
| Joomla          |     0|
| Clojure         |     0|
| Scala           |     2|
| Node js         |     4|
| Ruby on Rails   |     3|

``` r
encuestas$TrabajaDesdeCasa <-
    factor(
        encuestas$TrabajaDesdeCasa,
        levels = c("True", "False"),
        labels = c("Si", "No")
    )
kable(as.data.frame(table(encuestas$TrabajaDesdeCasa)), caption = "Cantidad de registros por trabaja desde casa")
```

| Var1 |   Freq|
|:-----|------:|
| Si   |   4113|
| No   |  17125|

``` r
encuestas$LeGustaTrabajarDesdeCasa <-
    factor(
        encuestas$LeGustaTrabajarDesdeCasa,
        levels = c("True", "False"),
        labels = c("Si", "No")
    )
kable(as.data.frame(table(encuestas$LeGustaTrabajarDesdeCasa)), caption =
          "Cantidad de registros por le gusta trabajar desde casa")
```

| Var1 |   Freq|
|:-----|------:|
| Si   |  14430|
| No   |   6808|

``` r
encuestas$CambioPorMejorSalario <-
    factor(
        encuestas$CambioPorMejorSalario,
        levels = c("True", "False"),
        labels = c("Si", "No")
    )
kable(as.data.frame(table(encuestas$CambioPorMejorSalario)), caption = "Cantidad de registros por cambiaria por mejor salario")
```

| Var1 |   Freq|
|:-----|------:|
| Si   |  16792|
| No   |   4446|

``` r
encuestas$CambioPorMejorAmbiente <-
    factor(
        encuestas$CambioPorMejorAmbiente,
        levels = c("True", "False"),
        labels = c("Si", "No")
    )
kable(as.data.frame(table(encuestas$CambioPorMejorAmbiente)), caption =
          "Cantidad de registros por cambiaria por ambiente")
```

| Var1 |   Freq|
|:-----|------:|
| Si   |   5744|
| No   |  15494|

``` r
encuestas$CambioPorFormaDeTrabajo <-
    factor(
        encuestas$CambioPorFormaDeTrabajo,
        levels = c("True", "False"),
        labels = c("Si", "No")
    )
kable(as.data.frame(table(encuestas$CambioPorFormaDeTrabajo)), caption =
          "Cantidad de registros por cambiaria por forma de trabajo")
```

| Var1 |   Freq|
|:-----|------:|
| Si   |   9145|
| No   |  12093|

``` r
encuestas$CambioPorTecnologia <-
    factor(
        encuestas$CambioPorTecnologia,
        levels = c("True", "False"),
        labels = c("Si", "No")
    )
kable(as.data.frame(table(encuestas$CambioPorTecnologia)), caption = "Cantidad de registros por cambiaria por tecnología")
```

| Var1 |   Freq|
|:-----|------:|
| Si   |   6199|
| No   |  15039|

``` r
encuestas$CambioPorCercania <-
    factor(
        encuestas$CambioPorCercania,
        levels = c("", "True", "False"),
        labels = c("No informa", "Si", "No")
    )
kable(as.data.frame(table(encuestas$CambioPorCercania)), caption = "Cantidad de registros por cambiaria por cercania")
```

| Var1       |   Freq|
|:-----------|------:|
| No informa |   4622|
| Si         |   4326|
| No         |  12290|

``` r
encuestas$CambioPorMenorCargaHoraria <-
    factor(
        encuestas$CambioPorMenorCargaHoraria,
        levels = c("", "True", "False"),
        labels = c("No informa", "Si", "No")
    )
kable(as.data.frame(table(encuestas$CambioPorMenorCargaHoraria)), caption =
          "Cantidad de registros por cambiaria por menor carga horaria")
```

| Var1       |   Freq|
|:-----------|------:|
| No informa |   4622|
| Si         |   3977|
| No         |  12639|

``` r
encuestas$CambioPorOportunidadDeCarrera <-
    factor(
        encuestas$CambioPorOportunidadDeCarrera,
        levels = c("", "True", "False"),
        labels = c("No informa", "Si", "No")
    )
kable(as.data.frame(table(
    encuestas$CambioPorOportunidadDeCarrera
)), caption = "Cantidad de registros por cambiaria por oportunidad de carrera")
```

| Var1       |  Freq|
|:-----------|-----:|
| No informa |  4622|
| Si         |  9029|
| No         |  7587|

``` r
encuestas$TienePersonasACargo <-
    factor(
        encuestas$TienePersonasACargo,
        levels = c("", "True", "False"),
        labels = c("No informa", "Si", "No")
    )
kable(as.data.frame(table(encuestas$TienePersonasACargo)), caption = "Cantidad de registros por tiene personas a cargo")
```

| Var1       |   Freq|
|:-----------|------:|
| No informa |   6171|
| Si         |   3978|
| No         |  11089|

Limpieza de valores anomalos
----------------------------

``` r
cantRegistrosAntesLimpieza <- dim(encuestas)[1]

# ---------------------- Limpieza de valores anomalos  ----------------------

# todas las (edad > 65 o < 18) las vamos a considerar anomalas y vamos a desechar esos registros
ggplot(data = encuestas, aes(Edad)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) + labs(title = "Histograma de edad - Antes de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos-1.png)

``` r
encuestas <- encuestas[encuestas$Edad < 66, ]
encuestas <- encuestas[encuestas$Edad > 17, ]

ggplot(data = encuestas, aes(Edad)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) + labs(title = "Histograma de edad - Después de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos-2.png)

``` r
cantAnomalosPorEdad <-
    cantRegistrosAntesLimpieza -  dim(encuestas)[1]

# todas las (horas trabajadas >= 100 o <= 18) las vamos a considerar anomalas.
# no vamos a desechar esos registros ya que son muchos, vamos a considerar que trabajan 40hs por semana
# que es la media para la variable
ggplot(data = encuestas, aes(horasTrabajadasXSemana)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) +
    labs(title = "Histograma de horas trabajadas por semana - Antes de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos-3.png)

``` r
encuestas[encuestas$horasTrabajadasXSemana >= 100, ]$horasTrabajadasXSemana <-
    40
encuestas[encuestas$horasTrabajadasXSemana <= 18, ]$horasTrabajadasXSemana <-
    40

ggplot(data = encuestas, aes(horasTrabajadasXSemana)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) +
    labs(title = "Histograma de horas trabajadas por semana - Después de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos-4.png)

``` r
cantAnomalosPorHorasTrabajadas <-
    cantRegistrosAntesLimpieza - cantAnomalosPorEdad -  dim(encuestas)[1]

# todos los (meses en el puesto actual >= 360) los vamos a considerar anomalas y vamos a desechar esos registros
ggplot(data = encuestas, aes(MesesEnElPuestoActual)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) +
    labs(title = "Histograma de meses en puesto actual - Antes de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos-5.png)

``` r
encuestas <- encuestas[encuestas$MesesEnElPuestoActual < 480, ]

ggplot(data = encuestas, aes(MesesEnElPuestoActual)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) +
    labs(title = "Histograma de meses en puesto actual - Después de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos-6.png)

``` r
cantAnomalosPorMesesEnPuestoActual <-
    cantRegistrosAntesLimpieza - cantAnomalosPorHorasTrabajadas - cantAnomalosPorEdad - dim(encuestas)[1]

# todos los (salario actual neto >= 150000 o <= 3000) los vamos a considerar anomalas y vamos a desechar esos registros
ggplot(data = encuestas, aes(SalarioActualNeto)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) +
    labs(title = "Histograma de Salario actual neto - Antes de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos-7.png)

``` r
encuestas <- encuestas[encuestas$SalarioActualNeto < 150000, ]
encuestas <- encuestas[encuestas$SalarioActualNeto > 3000, ]

ggplot(data = encuestas, aes(SalarioActualNeto)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) +
    labs(title = "Histograma de Salario actual neto - Después de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos-8.png)

``` r
cantAnomalosPorSalarioActualNeto <-
    cantRegistrosAntesLimpieza - cantAnomalosPorHorasTrabajadas - cantAnomalosPorEdad -      cantAnomalosPorMesesEnPuestoActual - dim(encuestas)[1]


# todos los (salario ideal neto >= 150000) los vamos a considerar anomalas y vamos a desechar esos registros
ggplot(data = encuestas, aes(SalarioIdealNeto)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) +
    labs(title = "Histograma de Salario ideal neto - Antes de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos-9.png)

``` r
encuestas <- encuestas[encuestas$SalarioIdealNeto < 150000, ]

ggplot(data = encuestas, aes(SalarioIdealNeto)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) +
    labs(title = "Histograma de Salario ideal neto - Después de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos-10.png)

``` r
cantAnomalosPorSalarioIdealNeto <-
    cantRegistrosAntesLimpieza - cantAnomalosPorHorasTrabajadas - cantAnomalosPorEdad - cantAnomalosPorMesesEnPuestoActual - cantAnomalosPorSalarioActualNeto - dim(encuestas)[1]


# Borramos los levels que no se usan
encuestas <- droplevels(encuestas)

cantRegistrosDespuesLimpieza <- dim(encuestas)[1]
```

Cantidad de registros anomalos por edad: 828
Cantidad de registros anomalos por horas trabajadas: 0
Cantidad de registros anomalos por meses en el puesto actual: 23
Cantidad de registros anomalos por salario actual neto: 1912
Cantidad de registros anomalos por salario ideal neto: 27

Cantidad de registros antes de limpieza: 21238
Cantidad de registros despues de limpieza: 18448
Cantidad de registros anomalos eliminados: 2790

Creación de caracteristicas
---------------------------

``` r
# ---------------------- Creación de caracteristicas  ----------------------

# Anio: Factor del año
encuestas$Anio <- factor(encuestas$Fecha$year + 1900)

# Mes: Factor del  Mes
encuestas$Mes <- factor(encuestas$Fecha$mon + 1)

# AnioMes: Factor del Año + Mes
encuestas$AnioMes <-
    factor((encuestas$Fecha$year + 1900) * 100 + encuestas$Fecha$mon + 1)

# Semestre: Factor del semestre
encuestas$Semestre <-
    cut(encuestas$Fecha$mon + 1,
        c(0, 6, 99),
        labels = c("Primer", "Segundo"))

# Hora: Factor de la hora
encuestas$Hora <- factor(encuestas$Fecha$hour)

# RangoHora: Factor por el momento del día de la encuesta
encuestas$RangoHora <-
    cut(
        encuestas$Fecha$hour,
        c(0, 6, 10, 14, 19, 24),
        labels = c("Madrugada", "Mañana", "Mediodia", "Tarde", "Noche")
    )

# DiferenciaSalarioRealIdeal: Diferencia entre el salario actual y el ideal
encuestas$DiferenciaSalarioRealIdeal <-
    encuestas$SalarioIdealNeto - encuestas$SalarioActualNeto

# SalarioNetoPorHora: Salario neto por hora trabajada por semana
encuestas$SalarioNetoPorHora <-
    encuestas$SalarioActualNeto / encuestas$horasTrabajadasXSemana

# RangoSalario: Rango del nivel remunerativo
encuestas$RangoSalario  <-
    cut(
        encuestas$SalarioNetoPorHora ,
        quantile(encuestas$SalarioNetoPorHora , c(0, 0.15, 0.30, 0.70, 0.90, 1)),
        labels = c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto")
    )

# Carga laboral: En relación a las horas que dedica al trabajo
encuestas$CargaLaboral <-
    cut(
        encuestas$horasTrabajadasXSemana,
        c(0, 35, 45, 50, 999),
        labels = c("Part Time", "Full Time", "Extra Time", "Very Extra Time")
    )

# Antiguedad: Nivel de antiguedad en el actual trabajo
encuestas$Antiguedad <-
    cut(
        encuestas$MesesEnElPuestoActual,
        c(0, 18, 36, 60, 999),
        labels = c("Junior", "SemiSenior", "Senior", "Expert")
    )

# Experiencia: Nivel de antiguedad en cualquier trabajo
encuestas$Experiencia <-
    cut(
        encuestas$MesesDeExperiencia,
        c(0, 25, 70, 110, 999),
        labels = c("Junior", "SemiSenior", "Senior", "Expert")
    )

# IdEdad: Rango de edad
encuestas$RangoEdad <-
    cut(encuestas$Edad, c(0, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65))
```

Limpieza de valores anomalos en caracteristicas creadas
-------------------------------------------------------

``` r
# ---------------------- Limpieza de valores anomalos en caracteristicas creadas  ----------------------

# Todas las diferencias de salario real e ideal mayores a 30000 y menores a -5000 las eliminamos
# Por considerarlas anomalas.
ggplot(data = encuestas, aes(DiferenciaSalarioRealIdeal)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) + labs(title = "Histograma de Diferencia entre salario real e ideal - Antes de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos%20en%20caracteristicas%20creadas-1.png)

``` r
encuestas <-
    encuestas[encuestas$DiferenciaSalarioRealIdeal < 30000, ]
encuestas <-
    encuestas[encuestas$DiferenciaSalarioRealIdeal > -5000, ]

ggplot(data = encuestas, aes(DiferenciaSalarioRealIdeal)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) +
    labs(title = "Histograma de Diferencia entre salario real e ideal - Después de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos%20en%20caracteristicas%20creadas-2.png)

``` r
cantAnomalosPorDiferenciaRealIdeal <-
    cantRegistrosDespuesLimpieza - dim(encuestas)[1]

# Todos los salarios netos por hora mayores a 1500 los vamos a eliminar por considerarlos anomalos
ggplot(data = encuestas, aes(SalarioNetoPorHora)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) +
    labs(title = "Histograma de Salario neto por hora - Antes de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos%20en%20caracteristicas%20creadas-3.png)

``` r
encuestas <- encuestas[encuestas$SalarioNetoPorHora < 1500, ]

ggplot(data = encuestas, aes(SalarioNetoPorHora)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(col = 2) +
    labs(title = "Histograma de Salario neto por hora - Después de limpieza")
```

![](main_files/figure-markdown_github/Limpieza%20de%20valores%20anomalos%20en%20caracteristicas%20creadas-4.png)

``` r
cantAnomalosPorSalarioPorHora <-
    cantRegistrosDespuesLimpieza - cantAnomalosPorDiferenciaRealIdeal - dim(encuestas)[1]

# Borramos los levels que no se usan
encuestas <- droplevels(encuestas)
```

Cantidad de registros anomalos por diferencia entre salario real e ideal : 485
Cantidad de registros anomalos por salario neto por hora: 23

Visualización de caracteristicas principales
--------------------------------------------

``` r
ggplot(encuestas, aes(x = Mes)) +
    geom_bar(aes(fill = Semestre)) +
    labs(title = "Cantidad de registros por mes")
```

![](main_files/figure-markdown_github/Visualización%20de%20caracteristicas%20principales-1.png)

``` r
# Vemos una gran cantidad de encuestas a principio de año y en el mes de septiembre. Esta lejos de ser una distribución pareja. Esto puede deverse a diversos motivos, como por ejemplo las fechas en las que se cierran paritarias o las fechas en donde se publican las encuestas.
# Consejo: Sería una buena idea intentar aplanar un poco más está distribución.

ggplot(encuestas, aes(x = Hora)) +
    geom_bar(aes(fill = RangoHora)) +
    labs(title = "Cantidad de registros por hora")
```

![](main_files/figure-markdown_github/Visualización%20de%20caracteristicas%20principales-2.png)

``` r
# Fuera de lo que se podria imaginar, hay una gran cantidad de registros que son a la madrugada. Esto puede indicar cierta anomalia en la recolección de la información. Tendremos que indagar un poco más la relación entre la valides teorica de la encuesta y la hora en la que se completó.
# Tambien puede ser por un desajuste en la hora del servidor de base de datos, entre otros motivos.

ggplot(encuestas, aes(x = Hora, y = Edad)) +
    geom_boxplot() +
    labs(title = "Relación entre hora de encuesta y Edad")
```

![](main_files/figure-markdown_github/Visualización%20de%20caracteristicas%20principales-3.png)

``` r
ggplot(encuestas, aes(x = Hora, y = SalarioNetoPorHora)) +
    geom_boxplot() +
    labs(title = "Relación entre hora de encuesta y Salario")
```

![](main_files/figure-markdown_github/Visualización%20de%20caracteristicas%20principales-4.png)

``` r
ggplot(encuestas, aes(x = Hora)) +
    geom_bar(aes(fill = IdNivelEducativo)) +
    labs(title = "Relacón entre hora de encuesta y nivel educativo")
```

![](main_files/figure-markdown_github/Visualización%20de%20caracteristicas%20principales-5.png)

``` r
# No parece haber una relación entre la hora y la edad o salario recibido o nivel educativo. Eso es bueno ya que es sintoma de una encuesta valida.

ggplot(encuestas, aes(x = SalarioActualNeto)) +
    geom_histogram(aes(fill = RangoSalario)) +
    facet_grid(. ~ Semestre) +
    labs(title = "Distribución de rangos salariales por semestre")
```

![](main_files/figure-markdown_github/Visualización%20de%20caracteristicas%20principales-6.png)

``` r
ggplot(encuestas, aes(x = SalarioNetoPorHora)) +
    geom_histogram(aes(fill = RangoSalario)) +
    facet_grid(. ~ Semestre) +
    labs(title = "Distribución de rangos salariales por semestre")
```

![](main_files/figure-markdown_github/Visualización%20de%20caracteristicas%20principales-7.png)

``` r
# Vemos que la grafica se corre a la derecha, producto de la inflación y los ajustes salariales

ggplot(encuestas, aes(x = Edad)) +
    geom_bar(aes(fill = RangoEdad)) +
    labs(title = "Cantidad de registros por Edad")
```

![](main_files/figure-markdown_github/Visualización%20de%20caracteristicas%20principales-8.png)

``` r
ggplot(encuestas, aes(x = horasTrabajadasXSemana)) +
    geom_bar(aes(fill = CargaLaboral)) +
    labs(title = "Cantidad de registros por horas trabajadas por semana")
```

![](main_files/figure-markdown_github/Visualización%20de%20caracteristicas%20principales-9.png)

``` r
ggplot(encuestas, aes(x = MesesEnElPuestoActual)) +
    geom_bar(aes(fill = Antiguedad)) +
    labs(title = "Cantidad de registros por meses en el puesto actual")
```

![](main_files/figure-markdown_github/Visualización%20de%20caracteristicas%20principales-10.png)

``` r
ggplot(encuestas, aes(x = MesesDeExperiencia)) +
    geom_bar(aes(fill = Experiencia)) +
    labs(title = "Cantidad de registros por meses de experiencia")
```

![](main_files/figure-markdown_github/Visualización%20de%20caracteristicas%20principales-11.png)

Exploración grafica
-------------------

Algunos graficos de ejemplo:

``` r
# distribución de sueldo netos por hora en función del puesto
bymedian <-
    with(encuestas, reorder(IdPuesto,-SalarioNetoPorHora, median))
bwplot(bymedian ~ SalarioNetoPorHora, encuestas, horizontal = TRUE)
```

![](main_files/figure-markdown_github/Exploración%20grafica-1.png)

``` r
# distribución de sueldo netos por hora en función del puesto
# para sueldos mayores de $500 por hora
bymedian <-
    with(
        subset(encuestas, SalarioNetoPorHora > 500),
        reorder(IdPuesto,-SalarioNetoPorHora, median)
    )
bwplot(
    bymedian ~ SalarioNetoPorHora,
    subset(encuestas, SalarioNetoPorHora > 500),
    horizontal = TRUE
)
```

![](main_files/figure-markdown_github/Exploración%20grafica-2.png)

``` r
# distribución de sueldo netos por hora en función del nivel educativo
bymedian <-
    with(encuestas,
         reorder(IdNivelEducativo,-SalarioNetoPorHora, median))
bwplot(bymedian ~ SalarioNetoPorHora, encuestas, horizontal = TRUE)
```

![](main_files/figure-markdown_github/Exploración%20grafica-3.png)

``` r
# distribución de sueldo netos por hora en función del sexo
bymedian <-
    with(encuestas, reorder(IdSexo,-SalarioNetoPorHora, median))
bwplot(bymedian ~ SalarioNetoPorHora, encuestas, horizontal = TRUE)
```

![](main_files/figure-markdown_github/Exploración%20grafica-4.png)

``` r
# distribución de sueldo netos por hora en función de las horas trabajadas por semana
bymedian <-
    with(encuestas, reorder(CargaLaboral,-SalarioNetoPorHora, median))
bwplot(bymedian ~ SalarioNetoPorHora, encuestas, horizontal = TRUE)
```

![](main_files/figure-markdown_github/Exploración%20grafica-5.png)

``` r
# distribución de sueldo netos por hora en función de la experiencia
bymedian <-
    with(encuestas, reorder(Experiencia,-SalarioActualNeto, median))
bwplot(bymedian ~ SalarioActualNeto, encuestas, horizontal = TRUE)
```

![](main_files/figure-markdown_github/Exploración%20grafica-6.png)

``` r
# distribución de sueldo netos por hora en función de la antiguedad
bymedian <-
    with(encuestas, reorder(Antiguedad,-SalarioActualNeto, median))
bwplot(bymedian ~ SalarioActualNeto, encuestas, horizontal = TRUE)
```

![](main_files/figure-markdown_github/Exploración%20grafica-7.png)

``` r
# distribución de sueldo netos por hora en función de la tecnologia
bymedian <-
    with(encuestas,
         reorder(IdTecnologiaPrincipal,-SalarioActualNeto, median))
bwplot(bymedian ~ SalarioActualNeto, encuestas, horizontal = TRUE)
```

![](main_files/figure-markdown_github/Exploración%20grafica-8.png)

``` r
# Se ve la relación entre sueldo y antiguedad alterada segun el sexo?
qplot(
    Edad,
    SalarioNetoPorHora,
    data = subset(encuestas, IdSexo != "No informa"),
    color = IdSexo,
    fill = IdSexo,
    geom = c("point", "smooth"),
    facets = . ~ Anio,
    method = "lm"
)
```

![](main_files/figure-markdown_github/Exploración%20grafica-9.png)

``` r
# Relación entre salario y sexo segun
qplot(
    SalarioNetoPorHora,
    data = encuestas,
    color = IdSexo,
    fill = IdSexo,
    facets = . ~ Anio
)
```

![](main_files/figure-markdown_github/Exploración%20grafica-10.png)

``` r
qplot(
    SalarioNetoPorHora,
    data = encuestas,
    color = IdSexo,
    fill = IdSexo,
    facets = . ~ IdNivelEducativo
)
```

![](main_files/figure-markdown_github/Exploración%20grafica-11.png)

``` r
# ----------------- ------------- ---------------

encuestas2016 <-
    subset(encuestas, as.POSIXlt(Fecha)$year + 1900 == 2016)

encuestasDesarrolladores <-
    subset(
        encuestas2016,
        IdPuesto == "Desarrollador de software / Programador" |
            IdPuesto == "Arquitecto"
    )
encuestasDesarrolladores <-
    subset(encuestasDesarrolladores, IdProvincia == "Capital Federal")

encuestasDesarrolladores[encuestasDesarrolladores$Semestre == "Primer", ]$SalarioActualNeto <-
    encuestasDesarrolladores[encuestasDesarrolladores$Semestre == "Primer", ]$SalarioActualNeto * 1.35
encuestasDesarrolladores[encuestasDesarrolladores$Semestre == "Segundo", ]$SalarioActualNeto <-
    encuestasDesarrolladores[encuestasDesarrolladores$Semestre == "Segundo", ]$SalarioActualNeto * 1.15

encuestasDesarrolladoresNet <-
    subset(encuestasDesarrolladores, IdTecnologiaPrincipal == ".Net")

# Distribución salarial por experiencia, llevado a valores de marzo 2017, para desarrolladores y arquitectos en .net
bymedian <-
    with(encuestasDesarrolladores,
         reorder(Experiencia,-SalarioActualNeto, median))
bwplot(
    bymedian ~ SalarioActualNeto,
    encuestasDesarrolladores,
    horizontal = TRUE,
    main = "Distribución salarial por experiencia, llevado a valores de marzo 2017, para desarrolladores y arquitectos"
)
```

![](main_files/figure-markdown_github/Exploración%20grafica-12.png)

``` r
# Distribución salarial por experiencia, llevado a valores de marzo 2017, para desarrolladores y arquitectos en .net
bymedian <-
    with(encuestasDesarrolladoresNet,
         reorder(Experiencia,-SalarioActualNeto, median))
bwplot(
    bymedian ~ SalarioActualNeto,
    encuestasDesarrolladoresNet,
    horizontal = TRUE,
    main = "Distribución salarial por experiencia, llevado a valores de marzo 2017, para desarrolladores y arquitectos en .net"
)
```

![](main_files/figure-markdown_github/Exploración%20grafica-13.png)

``` r
ggplot(encuestasDesarrolladores, aes(x = SalarioActualNeto)) +
    geom_histogram(aes(fill = Experiencia)) +
    labs(title = "Distribución de rangos salariales por semestre, para desarrolladores y arquitectos, a valores de marzo 2017")
```

![](main_files/figure-markdown_github/Exploración%20grafica-14.png)

``` r
ggplot(encuestasDesarrolladoresNet, aes(x = SalarioActualNeto)) +
    geom_histogram(aes(fill = Experiencia)) +
    labs(title = "Distribución de rangos salariales por semestre, para desarrolladores y arqui .net, a valores de marzo 2017") 
```

![](main_files/figure-markdown_github/Exploración%20grafica-15.png)
