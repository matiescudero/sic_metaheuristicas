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
  
  ## Se lee la instancia
  
  if (entrada == "instancia_g08.dat"){
    
    instancia = DatToInstance(entrada, 200, 48)
  }
  
  if (entrada == "instancia_i09.dat"){
    
    instancia = DatToInstance(entrada, 415, 45)
  }
  
  if (entrada == "instancia_c01.dat"){
    
    instancia = DatToInstance(entrada, 243, 56)
  }
  
  # Tamaño del problema
  len_sol = length(instancia$wj)
  
  ## Se evalua el máximo SIC
  xj_base = GenerateInitialSolution(instancia, 0)
  
  max_sic = EvaluateSIC(instancia, xj_base)
  
  ## Se genera la solución inicial
  xj_ini = GenerateInitialSolution(instancia, 10)
  
  ## Se calcula la interacción espacial para cada ejecución
  resultados_sa = SimulatedAnnealing(instancia, xj_ini, operador, 200, max_iter_interna, alpha)
  
  ## Se compara con el valor base
  resultado = max_sic - resultados_sa$spatial_interaction

  return(list(cost = resultado))
}

#======================
#Configuración de irace
#======================

# Lectura de scenario
escenario = readScenario(filename = "Tuning/scenario_sa.txt", scenario = defaultScenario())

# Lectura de parámetros
parametros = readParameters(file =  "Tuning/parameters_sa.txt")

escenario$targetRunner=target.runner

irace(scenario = escenario, parameters = parametros)

