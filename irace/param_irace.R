#======================
#Bibliotecas
#======================
library("RPostgres")
library("DBI")
library("sf")
library("tidyr")
library("textshape")
library("ggplot2")
library("irace")

setwd("C:/Users/mbell/Desktop/UNIVERSIDAD/DOCTORADO/2022-2/METAHEURISTICAS/sic_metaheuristicas/irace")
source("SIC_script.R")

#=====================
# Definición del runner
#=====================

target.runner = function(experiment, scenario){
  
  
  set.seed(1)
  
  #Ajuste de entrada
  entrada=experiment$instance
  entrada=strsplit(entrada,"/")
  entrada=entrada[[1]][length(entrada[[1]])]
  
  #Otros parámetros
  max_iter_interna=experiment$configuration[["max_iter_interna"]]
  max_iter=as.numeric(experiment$configuration[["max_iter"]])
  alpha=as.numeric(experiment$configuration[["alpha"]])
  operador=(experiment$configuration[["operador"]])
  

  #Previo a s.a
  
  #Se lee la instancia
  instancia = DatToInstance(entrada)
  ## Se genera la solución inicial
  xj_ini = GenerateInitialSolution(instancia, 95)
  
  resultado=SimulatedAnnealing(instancia,xj_ini, operador,max_iter, max_iter_interna, alpha)

  return(list(cost = resultado$spatial_interacion))
}

#======================
#Configuración de irace
#======================

# Lectura de scenario
escenario = readScenario(filename = "Tuning/scenario.txt", scenario = defaultScenario())

# Lectura de parámetros
parametros = readParameters(file =  "Tuning/parameters.txt")

escenario$targetRunner=target.runner

irace(scenario = escenario, parameters = parametros)
