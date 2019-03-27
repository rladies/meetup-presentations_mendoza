#' ---
#' title: "Taller R-Ladies - procesando el dataset de mujeres programadoras"
#' author: "Ana Laura Diedrichs"
#' date: "26 de marzo de 2019"
#' output: 
#'   html_document:
#'    toc: TRUE
#' ---
#'  Primer paso, carga de datos
#' ---
#'  
#'  Descargue el dataset en formato .csv desde el sitio 
#'  http://mujeresprogramadoras.com.ar/
#'  
#'  Para instalar la librería readr, descomente y ejecute la siguiente línea
#   install.packages("readr")
#' cargar la librería
#' install.packages("rmarkdown")

library(readr)
library(dplyr)
library(tidyr)
#' 
#' Instale el paquete ggplot usando
#' install.packages(ggplot2)
#' http://www.cookbook-r.com/Graphs/
library(ggplot2)
#' El primer paso es cargar el dataset.
#' 
#' Asegúrese que su working directory o directorio de trabajo esté en el mismo lugar que el dataset.
#' Puede configurarlo desde el botón More (símbolo engranaje ) o con el comando setwd("path/al/directorio/del/dataset")

#' Para visualizar en una pestaña separada, tipee `View(dataset)` en consola
#' 
dataset <- read_csv("Todas.las.carreras27032018.csv", locale=locale(encoding = "utf8"))
#'
#' ---
#'  Explorando el dataset mujeres programadoras
#' ---
#'  
#' Un resumen de la información por cada campo/variable del dataset
summary(dataset)

#' Nombres de los campos del dataset
#' 
colnames(dataset)

#' ¿Cuántos campos o columnas tiene el dataset?

ncol(dataset)
#' ¿Cuántas filas?

nrow(dataset)

#' Tipos de institución, sin repetir
unique(dataset$`Tipo de Institución`)

#' El valor NA indica Not available o no disponible y simboliza los datos perdidos en R

#' Listamos nombres de instituciones, sin repetir
unique(dataset$Institución)

#' Listamos unidad académica, sin repetir
unique(dataset$`Unidad Académica`)

#' ¿Qué observa de ejecutar las siguientes líneas?
 
dataset[1710:nrow(dataset),]

#' ¿Qué nos muestra la siguiente línea?
#' Puede que tenga que instalar la librería stats
complete.cases(dataset)

#' Puede usar la ayuda desde consola usando ?nombrefuncion, por ejemplo, para saber más
#' de la función complete.cases puede tipear en la consola ?complete.cases y luego enter.

#' ---
#'  Quitando valores perdidos al dataset 
#' ---

#' COMIENZA La limpieza del dataset. 
#' 
#' Quito la última columna. 
#' 
dataset <- dataset[,-ncol(dataset)]
# ¿Ahora qué nos muestra lo siguiente (descomentar y/o ejecutar)?
# complete.cases(dataset)
dataset[!complete.cases(dataset),]
#' Quito la última fila
head(dataset[-nrow(dataset),])
#' Para quitar las filas que contengan algún NA puede ejecutar lo siguiente
dataset[complete.cases(dataset),]

# Lo mismo puede ser realizado con dplyr usando lo siguiente:


dataset <- dataset %>%  
  select(-one_of("X21")) %>% 
  filter(complete.cases(.))

#' ---
#'  Calculando totales de mujeres vs varones
#' ---

#' prefiltramos y mostramos total estudiantes hombres vs mujeres
#' 

#' usamos `select` para indicar cuales columnas permanecen 
dataset %>% 
  select(Año,`Estudiantes Varones`,`Estudiantes Mujeres`) %>%
  group_by(Año) %>%
  summarise(
    TotalMujeres = sum(`Estudiantes Mujeres`,na.rm = TRUE),
    TotalVarones = sum(`Estudiantes Varones`,na.rm = TRUE)
  ) 
#' Agregamos una nueva columna como cálculo de la tasa de mujeres anual como $ TotalMujeres/TotalVarones * 100 $
#' 
datos <- dataset %>% 
  select(Año,`Estudiantes Varones`,`Estudiantes Mujeres`) %>%
  group_by(Año) %>%
  summarise(
    TotalMujeres = sum(`Estudiantes Mujeres`,na.rm = TRUE),
    TotalVarones = sum(`Estudiantes Varones`,na.rm = TRUE)
  ) %>%
 mutate(tasa = (TotalMujeres /(TotalVarones+TotalMujeres)) * 100 )


#' Funcionalidad de gather o spread 
dataset %>% 
  select(Año,`Estudiantes Varones`,`Estudiantes Mujeres`) %>%
  group_by(Año) %>%
  summarise(
    TotalAnioMujeres = sum(`Estudiantes Mujeres`,na.rm = TRUE),
    TotalAnioHombres = sum(`Estudiantes Varones`,na.rm = TRUE)
  ) %>%
  gather(key = genero, value = measurement,
         TotalAnioHombres,TotalAnioMujeres ) 

#' ---
#'  Extrayendo la informacion de Mendoza 
#' ---


#' ¿Qué nos regresa el siguiente comando?
#' 
#' ¿Univ. Mendoza, FRM UTN o Uncuyo?
#' Puedes encerrar toda la sentencia en View(....) para visualizar el dataset en una pestaña aparte en Rstudio.
#' Tenga en cuenta este consejo para las siguientes líneas.
dataset[grepl(dataset$Institución,pattern = "Mendoza"),]

#' ¿Qué nos regresa este comando?
#' ¿Univ. Mendoza, FRM UTN o Uncuyo?

dataset[grepl(dataset$`Unidad Académica`,pattern = "Mendoza"),]

#' ¿Qué nos regresa este comando?
#' ¿Univ. Mendoza, FRM UTN o Uncuyo?
dataset[grepl(dataset$Institución,pattern = "Cuyo"),]

#' Analizamos estudiantes, por institución, año en Mendoza.

#' Elegimos las columnas
datos <- dataset %>% 
  select(Año,Institución, `Unidad Académica`,Carrera,Gestión,`Estudiantes Varones`,`Estudiantes Mujeres`)
 
#' Filtramos las filas
#' ¿Qué universidades nos quedan?

dd <- datos  %>% 
  filter(grepl("Mendoza",`Unidad Académica`) | grepl("Mendoza",Institución))


#' También podemos poner todo junto
datos <- dataset %>% 
  select(Año,Institución, `Unidad Académica`,Carrera,Gestión,`Estudiantes Varones`,`Estudiantes Mujeres`) %>%
  filter(grepl("Mendoza",`Unidad Académica`) | grepl("Mendoza",Institución)| grepl("Cuyo",Institución))

#' Podemos seguir filtrando, por ejemplo, elegir un par de años, etc
datos <- dataset %>% 
  select(Año,`Estudiantes Varones`,`Estudiantes Mujeres`) %>%
  group_by(Año) %>%
  summarise(
    TotalAnioMujeres = sum(`Estudiantes Mujeres`,na.rm = TRUE),
    TotalAnioHombres = sum(`Estudiantes Varones`,na.rm = TRUE)
  )
#' A continuación algunas gráficas
#' 
ggplot(datos, aes(Año)) + 
  geom_line(aes(y = TotalAnioMujeres , colour = "mujeres")) + 
  geom_line(aes(y = TotalAnioHombres, colour = "hombres"))
# juntando preprocesamiento y mostrar un grafico al final
#' ejemplo 1 
dataset %>% 
  select(Año,`Estudiantes Varones`,`Estudiantes Mujeres`) %>%
  group_by(Año) %>%
  summarise(
    TotalAnioMujeres = sum(`Estudiantes Mujeres`,na.rm = TRUE),
    TotalAnioHombres = sum(`Estudiantes Varones`,na.rm = TRUE)
  ) %>%
ggplot(aes(Año)) + 
  geom_line(aes(y = TotalAnioMujeres , colour = "mujeres")) + 
  geom_line(aes(y = TotalAnioHombres, colour = "hombres"))


datos2 <- dataset %>% 
  select(Año,`Estudiantes Varones`,`Estudiantes Mujeres`) %>%
  group_by(Año) %>%
  summarise(
    TotalAnioMujeres = sum(`Estudiantes Mujeres`,na.rm = TRUE),
    TotalAnioHombres = sum(`Estudiantes Varones`,na.rm = TRUE)
  ) %>%
 gather(key = genero, value = measurement,
       TotalAnioHombres,TotalAnioMujeres ) 

#' Gráfico barras totales hombres y mujeres estudiantes de Mendoza por año
ggplot(datos2, aes(x=Año, y=measurement, fill=genero)) +
  geom_bar(stat='identity', position='dodge')

ggplot(datos2, aes(x=Año, y=measurement, fill=genero)) +
  geom_bar(stat='identity', position='dodge') + 
  facet_wrap(~ genero)
