#======================
#Bibliotecas
#======================
library("ggplot2")
library("irace")
source("QAP_script.R")

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
  N=experiment$configuration[["N"]]
  parada=as.numeric(experiment$configuration[["parada"]])
  T=as.numeric(experiment$configuration[["T"]])
  alpha=as.numeric(experiment$configuration[["alpha"]])
  operador=as.numeric(experiment$configuration[["operador"]])
  

  resultado=simmulated_annealing(entrada,N,parada,T,alpha,operador)

  return(list(cost =resultado))
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