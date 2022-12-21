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
  n_miembros=as.numeric(experiment$configuration[["n_miembros"]])
  max_iter=as.numeric(experiment$configuration[["max_iter"]])
  prob_mutacion=as.numeric(experiment$configuration[["prob_mutacion"]])
  operador=(experiment$configuration[["operador"]])
  

  #Previo a g.a
  
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
  
  ## Se calcula la interacción espacial para cada ejecución
  resultados_ga = GeneticAlgorithm(instancia, n_miembros, operador, 8, 300, prob_mutacion)
  
  ## Se compara con el valor base
  resultado = max_sic - resultados_ga$sic

  return(list(cost = resultado))
}

#======================
#Configuración de irace
#======================

# Lectura de scenario
escenario = readScenario(filename = "Tuning/scenario_ga.txt", scenario = defaultScenario())

# Lectura de parámetros
parametros = readParameters(file =  "Tuning/parameters_ga.txt")

escenario$targetRunner=target.runner

irace(scenario = escenario, parameters = parametros)

