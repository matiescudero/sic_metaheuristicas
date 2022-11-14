####################
#### FUNCIONES #####
####################

DatToInstance = function(nombre_archivo){
  #' Lee un archivo DAT y extrae los vectores y los almacena en una lista como instancia.
  #' 
  #' @param nombre_archivo (character) Nombre del archivo de extensión .DAT
  #' 
  #' @return instancia (list) Instancia que contiene dij, ai, ni y wj. 
  
  nombre_archivo = paste("Instancias/", nombre_archivo, sep = "")
  
  # Se lee el archivo dat y se almacena como DF
  contenido_dat = read.delim(nombre_archivo, header=FALSE, sep =" ")
  
  # Se extrae la matriz
  dij = contenido_dat[(2:527),(2:100)]
  dij = as.matrix(dij)
  class(dij) = "numeric"
  colnames(dij) = contenido_dat[1,1:99]
  rownames(dij) = contenido_dat[2:527,1]
  
  # Se elimina la matriz del contenido dat
  contenido_dat = contenido_dat[-c(1:528),]
  
  # Se extrae el vector ai
  ai = as.numeric(contenido_dat[(1:526),2])
  
  # Se elimina el vector ai
  contenido_dat = contenido_dat[-c(1:527),]
  
  # Se extrae el vector ni
  ni = as.numeric(contenido_dat[(1:526),2])
  
  # Se elimina el vector ni
  contenido_dat = contenido_dat[-c(1:527),]
  
  # Se extrae el vector wj
  wj = as.numeric(contenido_dat[(1:99),2])
  
  # Se almacenan los vectores en una lista
  instancia = list(dij = dij, ai = ai, ni = ni, wj = wj)
  return(instancia)
}

# Generar solución inicial aleatoria

GenerateInitialSolution = function(instancia, p){
  #' Genera el vector Xj inicial de forma aleatoria, indicando el n° p de paraderos a seleccionar.
  #' Además, se incluye el nombre de cada paradero en el vector generado. 
  #'
  #' @param instancia (list) Instancia que contiene dij, ai, ni y wj. 
  #' @param p (int) Número de paraderos a localizar. Debe ser igual o menor a n.
  #'
  #' @return xj (array) Vector de solución generado aleatoriamente
  
  # Número de paraderos disponibles
  n = length(instancia$wj)
  
  # Vector de solución aleatoria
  xj = c(rep(0, n - p), rep(1, p))
  xj = sample(xj)
  
  # Se indica el id de cada paradero para el vector generado
  names(xj) = colnames(instancia$dij)
  
  
  return(xj)
}


## Operadores ----


GetSwapNumbers = function(xj){
#' Obtiene las posiciones para realizar el operador swap. Si al evaluar estas posiciones dentro del
#' vector xj son idénticas se obtendrán nuevos números hasta que dejen de ser iguales.
  
  n = length(xj)
  is_equal = TRUE
  
  while(is_equal){
    
    n_swap = sample(1:n, 2)
    
    if(xj[n_swap[1]] != xj[n_swap[2]]){
      is_equal = FALSE
    }
  }
  
  return(n_swap)
}

Swap<-function(sol,i,j){
  
  piv<-sol[i]
  sol[i]<-sol[j]
  sol[j]<-piv
  return(sol)
}


SplitSolution = function(xj){
  #' Separa un vector de solución en un vector de ceros y otro de unos.
  #' 
  #' @param xj (array) Vector de solución.
  #' 
  #' @return xj_list (list) Lista de arrays, contiene los vectores xj_zeros y xj_ones
  #' 
  
  xj_zeros = xj[xj == 0]
  xj_ones = xj[xj == 1]
  
  xj_list = list(zeros = xj_zeros, ones = xj_ones)
  
  return(xj_list)
}


SwapSplit = function(xj_list){
  
  xj_zeros = xj_list$zeros
  xj_ones = xj_list$ones
  
  # Se escoge un elemento al azar de ambas listas y se guardan sus nombres 
  
  paradero_zero = names(sample(xj_zeros, 1))
  paradero_one = names(sample(xj_ones, 1))
  
  # Se cambian los nombres 
  
  names(xj_list$zeros)[names(xj_zeros) == paradero_zero] = paradero_one
  names(xj_list$ones)[names(xj_ones) == paradero_one] = paradero_zero
  
  # Pasar listas solo a una y ordenarla
  xj = c(xj_list$zeros, xj_list$ones)
  xj = xj[order(names(xj))]
  
  return(xj)
  
}

EvaluateSIC = function(instancia, xj){
  #' Evalua la interacción espacial entre un nodo de demanda i (zona censal) y un paradero j.
  #' 
  #' @param instancia (list) Lista que incluye la matriz dij y los vectores ai, ni y wj
  #' @param xj (array) Vector de solución que indica que paraderos j son localizados y cuales no
  #' 
  #' @return acum (int) Valor de interacción espacial entre el nodo i y el paradero j
  #'     
  
  ## Se almacenan las entradas en variables
  dij = instancia$dij
  ai = instancia$ai
  ni = instancia$ni
  wj = instancia$wj
  
  ## Se inicializa la suma y los n's
  acum = 0
  n_zonas = length(ai)
  n_paraderos = length(wj)
  
  ## Ciclo para evaluar el modelo SIC
  for(i in 1:n_zonas){
    for(j in 1:n_paraderos){
      
      ## Valor de Sij para un par (i,j)
      sij = ((wj[j]**2 * dij[i,j]**-2)/(ni[i]))*ai[i]*xj[[j]]
      acum = acum + sij
      
    }
  }
  
  return(acum)
}


CalculateInitialTemperature = function(instancia, xj, spatial_interaction, p0){
  
  
  ## Se inicializan las listas
  lista_deltas = numeric(100)
  lista_si = numeric(100)
  
  ## Se inicializa xj y si
  xj_temp = xj
  si_temp = spatial_interaction
  
  # largo de la solución
  n = length(xj_temp)
  
  for(i in 1:100){
    
    ## Se llena la lista de spatial interaction
    lista_si[i] = si_temp
    
    ## Se aplica operador sobre la solución inicial
    n_swap = GetSwapNumbers(xj_temp)
    xj_temp = Swap(xj_temp, n_swap[1], n_swap[2])
    
    
    ## Para cada ciclo se evalúa el vector solución generado 
    nuevo_si = EvaluateSIC(instancia, xj_temp)
    
    ## Se calcula el delta de la interacción espacial
    delta_si = nuevo_si - si_temp
    
    ## Se almacena el delta
    lista_deltas[i] = delta_si
    
    ## El SI actual pasa a ser el nuevo
    si_temp = nuevo_si
    
  }
  
  ## Se calcula el delta promedio
  delta_promedio = mean(abs(lista_deltas))
  
  
  ## De acuerdo a la fórmula exp(-(delta_promedio)/t_inicial) = p0, despejando queda:
  t_inicial = -delta_promedio/log(p0)
  
  return(t_inicial)
}


CalculateInitialTemperature2 = function(instancia, xj, spatial_interaction, p0){
  
  
  ## Se inicializan las listas
  lista_deltas = numeric(100)
  lista_si = numeric(100)
  
  ## Se inicializa xj y si
  xj_temp = xj
  si_temp = spatial_interaction
  
  # largo de la solución
  n = length(xj_temp)
  
  # Se evalúa SIC 100 veces 
  xj_a = sapply(1:100, function(x) EvaluateSIC(instancia, Swap(xj_temp, sample(1:n,1), sample(1:n,1))))
  xj_b = sapply(1:100, function(x) EvaluateSIC(instancia, Swap(xj_temp, sample(1:n,1), sample(1:n,1))))
  
  ## Se calcula el delta promedio
  delta_promedio = mean(abs(xj_a - xj_b))
  
  ## De acuerdo a la fórmula exp(-(delta_promedio)/t_inicial) = p0, despejando queda:
  t_inicial = -delta_promedio/log(p0)
  
  return(t_inicial)
}



SimulatedAnnealing = function(instancia, xj_ini, operador, max_iter, max_iter_interna, alpha){
  #' Calcula el menor costo al aplicar el algoritmo de S.A a una función objetivo dada.
  #' 
  #' @param instancia (list) Lista que incluye la matriz dij y los vectores ai, ni y wj
  #' @param xj_ini (array) Vector que contiene una configuración inicial de Xj para el modelo SIC
  #' @param operador (character) Nombre del operador a aplicar. Puede ser "swap" o "swap_split"
  #' @param max_iter (int) Número de iteraciones máximas a realizar por el ciclo externo del algoritmo.
  #' @param max_iter_interna (int) Número máximo de iteraciones a realizar por el ciclo interno del algoritmo.
  #' @param alpha (float) Factor de enfriamiento de la temperatura por cada nuevo ciclo
  #' 
  #' @return xj (array) mejor configuración de Xj encontrada
  #' @return spatial_interaction (float) máximo valor de interacción espacial encontrada para el modelo SIC
  #' @return eval_si (array) solución actual para cada iteración
  #' @return eval_mejor (array) mejor solución encontrada para cada iteración
  #' @return temp (array) evolución de la temperatura para cada iteración
  #'   
  
  ## Variables de tracking
  eval_si = numeric()
  eval_si_iter = numeric()
  eval_mejor = numeric()
  eval_iter = numeric()
  eval_time = numeric()
  temp = numeric()
  
  #Inicialización tiempo
  start_time = Sys.time()
  
  # Inicialización soluciones (xj)
  ## Se genera el vector de solución inicial 
  xj = xj_ini
  mejor_xj = xj
  
  ## Se obtiene un primer interacción espacial a partir de la evaluación de una solución random
  spatial_interaction = EvaluateSIC(instancia, xj)
  mejor_si = spatial_interaction
  
  ## Se obtiene la temperatura inicial
  t_inicial = CalculateInitialTemperature(instancia, xj, spatial_interaction, 0.5)
  t = t_inicial
  
  ## Contador para ciclo loop
  iter_externa = 1
  i_ciclo = 1
  n = length(xj)
  
  ## Ciclo Externo
  while(iter_externa < max_iter){
    
    ## Se inicializa la variable para la iteración interna
    iter_interna = 1
    while (iter_interna < max_iter_interna){
      
      ## Se selecciona el operador a utilizar
      
      if (operador == "swap"){
        
        n_swap = GetSwapNumbers(xj)
        xj_test = Swap(xj, n_swap[1], n_swap[2])
        
      }
      
      if (operador == "swap_split"){
        xj_list = SplitSolution(xj)
        xj_test = SwapSplit(xj_list)
        
      }
      
      # Se evalua el vector de solución
      si_test = EvaluateSIC(instancia, xj_test)
      
      # Se chequea si se reemplaza la solución
      delta_si = spatial_interaction - si_test
      
      if(exp(-(delta_si)/t) > runif(1)){ 
        
        xj = xj_test
        spatial_interaction = si_test
      }
      
      # Se actualiza la mejor solución
      if(si_test >= mejor_si){
        mejor_xj = xj_test
        mejor_si = si_test
      }
      
      # Variables de trackeo
      eval_si = c(eval_si, spatial_interaction)
      eval_mejor = c(eval_mejor, mejor_si)
      temp = c(temp, t)
      
      iter_interna = iter_interna + 1
    }
    
    # Se aplica un factor de enfriamiento de alpha (parámetro de la función)
    t = t*alpha
    
    eval_iter = c(eval_iter, iter_externa)
    eval_si_iter = c(eval_si_iter, spatial_interaction)
    
    iter_externa = iter_externa + 1
    
    print("----")
    print(iter_externa)
  }
  
  # Se finaliza el reloj
  
  end_time = Sys.time()
  time_iter = as.numeric(end_time - start_time)
  
  eval_time = c(eval_time, time_iter)
  
  print(paste("El mejor Resultado encontrado es:", mejor_si))
  
  resultados_sa = list(xj = mejor_xj, 
                       spatial_interaction = mejor_si, 
                       eval_si=eval_si, 
                       eval_mejor=eval_mejor,
                       temp = temp,
                       eval_iter = eval_iter,
                       eval_time = eval_time,
                       eval_si_iter = eval_si_iter)

  print(paste("El mejor Resultado encontrado es:", resultados_sa$spatial_interaction))
  
  return(mejor_si)
}

