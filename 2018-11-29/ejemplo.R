#' ---
#' title: "Taller R-Ladies - procesando el dataset de mujeres programadoras"
#' author: "Ana Laura Diedrichs"
#' date: "29 de noviembre de 2018"

#' ---
#'  Primer paso, carga de datos
#' ---

data(cars)

#' 
cars
#' La siguiente linea carga el dataset
suppressMessages(library(dplyr))

#' Estoy filtrando velocidades mayores a 20

cars %>% filter(speed > 20 )
#' ---
#'  Grafico
#' ---

plot(cars)

#' ---
#'  Modelo
#' ---

lm(speed~dist,cars)

plot(cars)
abline( lm(dist~speed,cars))
