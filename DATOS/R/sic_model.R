library(RPostgres)
library(DBI)
library(sf)
library(tidyr)
library(textshape)
library("ggplot2") 
library("reshape2") 

#### Credenciales DB ####

pg_db_parameters = list(driver = RPostgres::Postgres(),
                         db = 'metaheuristicas',
                         host = 'localhost',
                         port = '5432',
                         user = 'postgres',
                         passwd = 'postgres')



####################
#### FUNCIONES #####
####################

## BD a R ##

# Genera Conexión a BD

ConnectionToDb = function(db_parameters){
#' Genera un objeto 'PqConnection' de la clase "RPostgres" a partir de una lista con los parámetros
#' de conexión a una BD. 
#'
#' @param db_parameters (list) Lista de parámetros de conexión a una BD.
#'
#' @return connection (PqConnection) Objeto de clase "Rpostgres".
  
  driver = db_parameters$driver
  db = db_parameters$db
  host = db_parameters$host
  port = db_parameters$port
  user = db_parameters$user
  passwd = db_parameters$passwd
  
  connection = dbConnect(driver, db, host, port, user, passwd, options="-c search_path=output")
  
  return(connection)
}

# Almacenar tablas espaciales

PostgisToDf = function(connection, layer){
#' Transforma una capa espacial de la BD conectada a un Data Frame. 
#' 
#' @param connection (PqConnection) Objeto de clase "RPostgres".
#' @param layer (character) Nombre de la capa espacial a transformar.
#' 
#' @return df (data.frame) Data Frame de la capa espacial indicada.
  
  df = st_read(connection, layer)
  
  return(df)
}

# Transformar dataframe de distancias a matriz  dij_matrix = column_to


DfToMatrix = function(distance_df){
#' Transforma el formato de una matriz de distancias desde pares origen destino a una matriz
#' de distancia entre todos los paraderos y todos los nodos de demanda.
#' 
#' @param distance_df (data.frame) Data Frame que contiene la distancia entre todos los pares OD.
#' 
#' @return dij_matrix (matrix) Matriz de distancia entre todos los nodos de demanda i y los paraderos j.

  dij_matrix = spread(distance_df, stop_id, distance)
  dij_matrix = column_to_rownames(dij_matrix, "zc_id")
  dij_matrix = as.matrix(dij_matrix)
  return(dij_matrix)
}

# Generar instancia
MakeInstance = function(dij, ai, ni, wj){
#' Agrupa las entradas en una lista
#'
#' @param dij (matrix) Matriz de distancia entre todos los nodos i y todos los paraderos j
#' @param ai (array) Vector de demanda de los nodos i
#' @param ni (array) Vector que incluye la suma entre el producto entre dij*wj para todos los paraderos que se encuentran a 500 mts de un nodo
#' @param wj (array) Vector de pesos de los paraderos j
#' 
#' @return instancia (list) Lista con los valores estáticos de entrada
  
  instancia = list(dij = dij, ai = ai, ni = ni, wj = wj)
  return(instancia)
}



InstanceToDat = function(instancia, nombre_archivo){
#' Transforma la instancia en un archivo .DAT
#' 
#' @param instancia (list) Instancia que contiene dij, ai, ni y wj. 
  
  lapply(instancia, function(x) write.table( data.frame(x), paste("DATOS/DAT/",nombre_archivo,'.dat',sep = ""), append= T, sep=' '))
  
}


DatToInstance = function(nombre_archivo, n_nodos, n_paraderos){
#' Lee un archivo DAT y extrae los vectores y los almacena en una lista como instancia.
#' 
#' @param nombre_archivo (character) Nombre del archivo de extensión .DAT
#' 
#' @return instancia (list) Instancia que contiene dij, ai, ni y wj. 
  
  nombre_archivo = paste("DATOS/DAT/", nombre_archivo, sep = "")
  
  # Se lee el archivo dat y se almacena como DF
  contenido_dat = read.delim(nombre_archivo, header=FALSE, sep =" ")
  
  # Se extrae la matriz
  dij = contenido_dat[(2:(n_nodos + 1)),(2:(n_paraderos + 1))]
  dij = as.matrix(dij)
  class(dij) = "numeric"
  colnames(dij) = contenido_dat[1,1:n_paraderos]
  rownames(dij) = contenido_dat[2:(n_nodos + 1),1]
  
  # Se elimina la matriz del contenido dat
  contenido_dat = contenido_dat[-c(1:(n_nodos + 2)),]
  
  # Se extrae el vector ai
  ai = as.numeric(contenido_dat[(1:n_nodos),2])
  
  # Se elimina el vector ai
  contenido_dat = contenido_dat[-c(1:(n_nodos + 1)),]
  
  # Se extrae el vector ni
  ni = as.numeric(contenido_dat[(1:n_nodos),2])
  
  # Se elimina el vector ni
  contenido_dat = contenido_dat[-c(1:n_nodos + 1),]
  
  # Se extrae el vector wj
  wj = as.numeric(contenido_dat[(1:n_paraderos),2])
  
  # Se almacenan los vectores en una lista
  instancia = list(dij = dij, ai = ai, ni = ni, wj = wj)
  return(instancia)
}


# Generar solución inicial aleatoria

GenerateInitialSolution = function(instancia, p){
  #' Genera el vector Xj inicial de forma aleatoria, indicando el n° p de paraderos a eliminar.
  #' Además, se incluye el nombre de cada paradero en el vector generado. 
  #'
  #' @param instancia (list) Instancia que contiene dij, ai, ni y wj. 
  #' @param p (int) Número de paraderos a eliminar. Debe ser igual o menor a n.
  #'
  #' @return xj (array) Vector de solución generado aleatoriamente
  
  # Número de paraderos disponibles
  n = length(instancia$wj) 
  
  # Vector de solución aleatoria
  xj = c(rep(0, p), rep(1, n - p))
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

Swap = function(sol,i,j){
  
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
  
  print("xj_list:")
  print(xj_list)
  
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
  
  print("xj:")
  print(xj)
  
  xj = xj[order(names(xj))]
  
  return(xj)
  
}


TwoPointCrossover = function(parent1, parent2){
  #' Genera un cruzamiento de dos puntos entre dos padres y devuelve dos hijos.
  
  # largo del vector de solución
  n = length(parent1)
  
  ## 
  is_equal_sum = TRUE
  
  while (is_equal_sum){
    
    # Se eligen dos números entre 1 y n
    points = sort(sample(1:n, 2))
    
    ## se generan los vectores a cruzar
    piv1 = parent1[points[1]:points[2]]
    piv2 = parent2[points[1]:points[2]]
    
    
    ## Se chequea si es que la suma de los vectores es igual, sino se ejecuta de nuevo 
    if (sum(piv1) == sum(piv2)){
      
      parent1[points[1]:points[2]] = piv2
      parent2[points[1]:points[2]] = piv1
      
      is_equal_sum = FALSE
    } 
  }
  
  children = parent1
  
  return(children)
}

CrossoverManuel = function(padre1, padre2){
  #' Almacena las posciones de los ceros de los vectores padres, extrae al azar p ceros y genera un hijo.
  #' 
  #' @parameter padre1 (list): Vector de solución al problema de optimización que se cruza con el padre 2
  #' @parameter padre2 (list): Vector de solución al problema de optimización que se cruza con el padre 1
  #' 
  #' @return hijo (list): Vector resultante del cruzamiento entre los dos padres
  
  # Largo del vector
  len_padre = length(padre1)
  
  # Se genera una lista con unos
  hijo = padre1
  hijo[1:len_padre] = 1
  
  
  # Se almacenan las posiciones de los ceros de ambos vectores
  ceros_p1 = which(padre1 == 0)
  ceros_p2 = which(padre2 == 0)
  
  # Se almacena la cantidad de ceros de los vectores
  n_ceros = length(ceros_p1)
  
  # Se unen las posiciones 
  cero_padres = c(ceros_p1, ceros_p2)
  
  # Se eliminan los duplicados
  lista_ceros = unique(cero_padres)
  
  
  names(lista_ceros) = unique(names(cero_padres))
  
  # Se alojan los nombres de los paraderos que son ceros en los padres
  paraderos_cero = names(lista_ceros)
  
  # Se extraen al azar n_ceros paraderos de la lista de nombre de paraderos
  sample_paraderos = sample(paraderos_cero, n_ceros)
  
  # Se reemplazan los ceros en el hijo
  hijo[sample_paraderos] = 0
  
  return(hijo)
  
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


EvaluatePopulationSIC = function(instancia, poblacion){
  #' Calcula el SIC para cada elemento de una población y la probabilidad de selección de cada miembro
  #' 
  #' @param instancia (list) Lista que incluye la matriz dij y los vectores ai, ni y wj
  #' @param poblacion (matrix) matriz que contiene que contiene n vectores de solución
  #' 
  #' @return sic_poblacion (array) vector que contiene el SIC de cada uno de los miembros de la población
  #' @return prob_seleccion (array) Probabilidad de selección de cada miembro de la población
  #' 
  
  ## Número de miembros de la población
  n_miembros = ncol(poblacion)
  
  ## Se evalúa el SIC para cada vector de solución de la población dada
  sic_poblacion = apply(poblacion, 2, function(x) EvaluateSIC(instancia, x))
  
  ## Se almacena el mínimo y máximo SIC obtenido
  max_sic = max(sic_poblacion)
  min_sic = min(sic_poblacion)
  
  ## Se calcula la probabilidad de selección para cada miembro de la población
  if (max_sic == min_sic){
    
    ## Si es que los valores de SIC son iguales, la probabilidad de selección es la misma para todos.
    prob_seleccion = rep(1/n_miembros, n_miembros)
  } else {
    
    ## Si los valores de SIC son distintos, los que sean mejores tendrán una mayor probabilidad de selección
    suma_sic = sum(sic_poblacion)
    prob_seleccion = sic_poblacion/suma_sic
  }
  
  return(list(sic_poblacion = sic_poblacion,
              prob_seleccion = prob_seleccion))
}

GenerarPosiblesSoluciones <- function(n, p) {
  #' Genera todos los vectores posibles para un problema de largo n situando p paraderos.
  #' 
  #' @param n (int): Largo de los vectores a generar
  #' @param p (int): Número de paraderos a incluir para cada vector
  #' 
  #' @return 
  
  ind <- combn(seq_len(n), p)
  ind <- t(ind) + (seq_len(ncol(ind)) - 1) * n
  res <- rep(0, nrow(ind) * n)
  res[ind] <- 1
  res = matrix(res, ncol = n, nrow = nrow(ind), byrow = TRUE)
  return(res)
}

ObtenerMayorSIC = function(matriz_soluciones, instancia){
  #' Obtiene el mayor SIC dado una matriz de posibles soluciones.
  #' 
  #' @param matriz_soluciones (matrix): Matriz que contiene un conjunto de vectores de solución.
  #' @param instancia (list): Instancia sobre la cual se evalúa el SIC
  #' 
  #' @return max_sic (int): Máximo valor de SIC para el conjunto de vectores dado.
  
  # Tiempo
  start.time = Sys.time()
  
  # Lista que almacena todos los posibles valores de SIC para la matriz dada
  lista_sic = apply(matriz_soluciones, 1, function(x) EvaluateSIC(instancia, x))
  
  #Máximo valor de SIC para todos los vectores evaluados
  max_sic = max(lista_sic)
  
  end.time <- Sys.time()
  
  print(end.time - start.time)
  
  return(max_sic)
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

#####################
## METAHEURÍSTICAS ##
#####################

#### Simulated Annealing

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
  
  return(list(xj = mejor_xj, 
              spatial_interaction = mejor_si, 
              eval_si=eval_si, 
              eval_mejor=eval_mejor,
              temp = temp,
              eval_iter = eval_iter,
              eval_time = eval_time,
              eval_si_iter = eval_si_iter))
}

#### Genetic Algorithm

GeneticAlgorithm = function(instancia, n_miembros, operador, n_paraderos, max_iter, prob_mutacion){
  
  
  ## Tamaño de la solución del problema
  n = length(instancia$wj)
  
  # Evolución del mejor sic y tiempo de cada iteración
  evol = numeric(max_iter)
  eval_time = numeric()
  
  #Inicialización tiempo
  start_time = Sys.time()
  
  ## Se genera la población con la cantidad n_miembros 
  padres = replicate(n_miembros, GenerateInitialSolution(instancia, n_paraderos))
  
  ## Se inicializa la matriz que almacenará a los hijos
  hijos = replicate(n_miembros, numeric(n))
  
  # Se le asignan los nombres de los paraderos a los hijos
  rownames(hijos) = rownames(padres)
  
  ## Se inicializan las variables que almacenan el mejor sic y mejor vector de solución
  best_sic = -Inf
  best_sol = numeric(n)
  
  ## Ciclo que se repetirá max_iter veces
  
  iter = 1
  
  while (iter <= max_iter) {
    
    
    print(paste("Iteración número:",iter,"de",max_iter))
    
    ## Se evalúa el SIC para todos los miembros de la población
    evaluacion_sic = EvaluatePopulationSIC(instancia, padres)
    
    ## Se almacena el máximo valor encontrado para la iteración
    sic_iter = max(evaluacion_sic$sic_poblacion)
    
    ## Se almacena la posición del mejor resultado encontrado
    pos_max = which(evaluacion_sic$sic_poblacion == sic_iter)
    
    ## vector de mejor solución encontrada en el ciclo
    sol_iter = padres[,pos_max]
    
    # REEMPLAZO DE SOLUCIÓN
    
    # Se obtiene la posición de la peor solución y se cambia por la mejor
    pos_min = which(evaluacion_sic$sic_poblacion == min(evaluacion_sic$sic_poblacion))
    hijos[,pos_min] = best_sol
    
    # Se actualiza la mejor solución y mejor fitness
    if (sic_iter > best_sic){
      
      best_sol = sol_iter
      best_sic = sic_iter
    }
    
    ## Ciclo para generar hijos
    for (miembro in 1:n_miembros){
      
      ## Se seleccionan los padres en base a su probabilidad de selección
      n_padres = sample(1:n_miembros, 2, prob = evaluacion_sic$prob_seleccion)
      padre1 = padres[,n_padres[1]]
      padre2 = padres[,n_padres[2]]
      
      #Se genera un hijo mediante el cruzamiento de los padres
      
      if (operador == "two_point_crossover"){
        
        hijos[,miembro] = TwoPointCrossover(padre1, padre2)
        
      } else {
        
        hijos[,miembro] = CrossoverManuel(padre1, padre2)
        
      }
        
      #Se aplica un swap al hijo dada una probabilidad
      if (prob_mutacion > runif(1)){
        n_swap = GetSwapNumbers(hijos[,miembro])
        hijos[,miembro] = Swap(hijos[,miembro], n_swap[1], n_swap[2])
      }
    }
    
    # Los padres se convierten en hijos
    padres = hijos
    
    # Se almacena el mejor SIC
    evol[iter] = best_sic
    iter = iter + 1
  }
  
  # Se finaliza el reloj
  
  end_time = Sys.time()
  time_iter = as.numeric(end_time - start_time)
  
  # Se almacena el tiempo de cada iteración
  eval_time = c(eval_time, time_iter)
  
  return(list(sol = best_sol, sic = best_sic, evol = evol, eval_time = eval_time, padres = padres))
  
}


################
## Resultados ##
################

IterateSimulatedAnnealing = function(instancia, n_iteraciones, n_paraderos, operador, max_iter, max_iter_interna, alpha){
  
  #' Itera el algoritmo S.A las veces que se indique de acuerdo a los parámetros de ingreso.
  #' 
  #' @return eval_iter (list) Lista de 
  
  # Solución random inicial
  xj_ini = GenerateInitialSolution(instancia, n_paraderos)
  
  # Se inicializa vector para almacenar los resultados
  eval_iter = list()
  
  # Ciclo para ejecutar S.A n veces 
  for (k in 1:n_iteraciones){
    
    # Se alamacenan los resultados de una ejecución
    resultados_sa = SimulatedAnnealing(instancia,
                                       xj_ini,
                                       operador, 
                                       max_iter, 
                                       max_iter_interna, 
                                       alpha)
    
    print(paste("Iteración", k, "de", n_iteraciones, "para", n_paraderos, "paraderos utilizando operador swap"))
    
    # Se guarda cada una de las 11 iteraciones 
    eval_iter[[paste("iter", k, sep = "")]] = c(eval_iter[[paste("iter", k, sep="")]],
                                                resultados_sa)
  }
  
  return(eval_iter)
}


IterateGeneticAlgorithm = function(instancia, n_iteraciones, n_miembros, operador, n_paraderos, max_iter, prob_mutacion){
  
  # Se inicializa vector para almacenar los resultados
  eval_iter = list()
  
  # Ciclo para ejecutar S.A n veces 
  for (k in 1:n_iteraciones){
    
    # Se alamacenan los resultados de una ejecución
    resultados_ga = GeneticAlgorithm(instancia,
                                     n_miembros,
                                     operador,
                                     n_paraderos,
                                     max_iter,
                                     prob_mutacion)
    
    print(paste("Iteración", k, "de", n_iteraciones, "para", n_paraderos, "paraderos utilizando operador", operador))
    
    # Se guarda cada una de las 11 iteraciones 
    eval_iter[[paste("iter", k, sep = "")]] = c(eval_iter[[paste("iter", k, sep="")]],resultados_ga)
  }
                                                
  return(eval_iter)
}


GetSICAndTimeList = function(resultados_iteraciones, instancia, algoritmo){
#' Se obtiene una lista que contiene las listas de fitness y tiempo de las ejecuciones de S.A
#' 
#' @param resultados_iteraciones (list) Lista que contiene los resultados de distintas iteraciones de S.A.
#' 
#' @return list_time_sic (list) Lista que contiene las listas de SIC y tiempo de ejecución
  
  
  
  sic_list = numeric()
  time_list = numeric()
  padres_list = list()
  
  if (algoritmo == "SA"){
    
    for (iter in resultados_iteraciones){
      
      sic_list = c(sic_list, iter$spatial_interaction)
      time_list = c(time_list, iter$eval_time)
      
    }
    
  }
  
  if (algoritmo == "GA"){
    
    for (iter in resultados_iteraciones){
      
      sic_list = c(sic_list, iter$sic)
      time_list = c(time_list, iter$eval_time)
      
      #Para almacenar listas
      sic_padres = EvaluatePopulationSIC(instancia, iter$padres)$sic_poblacion 
      
      padres_list = c(padres_list, list(sic_padres))
    }
    
  }
  
  list_time_sic = list("sic" = sic_list, "time" = time_list, "sic_parents" = padres_list)
  
  return(list_time_sic)
  
}

PlotSIC = function(resultados_iteraciones, operador){
#' 
  
  for (iter in seq_along(resultados_iteraciones)){
    
    # Diferencia entre la máxima interacción espacial y la obtenida en cada iteración 
    dif_sic = max_sic - resultados_iteraciones[[iter]]$eval_si_iter
    
    sic = resultados_iteraciones[[iter]]$spatial_interaction
    
    dif_iter = round(max_sic - sic, 2) 
    
    jpeg(paste("DATOS/JPEG/", operador,"_sa_iter",iter,".jpg",sep = ""), width = 1000, height = 700)
    
    plot = plot(dif_sic, type = "l", col = "#63B389", lwd = 2,
                main = paste("39/45 paraderos\n S.I =",sic,"\nMínima diferencia =",dif_iter),
                xlab = "N° iteración",
                ylab = "Diferencia con S.I original")
    
    dev.off()
    
    
  }
  
}


ListsToDataFrame = function(listas_resultados, max_sic){
#' Transforma las listas de listas en un Data Frame para graficar  
  
  matrix = do.call(cbind, listas_resultados$sic_parents)
  df = as.data.frame(max_sic - matrix)
  n_col = length(df)
  col_names = c(1:n_col)
  colnames(df) = col_names

  return(df)
}
  
HijosABoxplot = function(ga_df){
  #' Genera un gráfico de boxplot para cada iteración 
  #'   
  
  # Se cambia el formato del data frame de entrada
  ga_df_long = melt(ga_df)
  
  # Se genera el gráfico de boxplot para los mejores hijos
  boxplot = ggplot(ga_df_long, aes(x = variable, y = value)) +
    geom_boxplot(fill = "#63B389") +
    ggtitle("Diferencia SIC para últimos hijos de cada iteración") +
    xlab("N° iteración") +
    ylab("Diferencia SIC")
  
  return(boxplot)
}   

ListaResultadoADfPlot = function(lista_resultados, algoritmo){
  
  if (algoritmo == "SA"){
    
    df = as.data.frame(do.call(cbind, lista_resultados))
    df$algoritmo = "S.A"
  }
  
  if (algoritmo == "GA"){
    
    df = as.data.frame(do.call(cbind, lista_resultados[1:2]))
    df$algoritmo = "G.A"
    
  }
  
  df$iter = c(1:nrow(df))
  
  return(df)
}

###########################
## CREACIÓN INSTANCIAS ####
###########################

## Conexión a BD

con = ConnectionToDb(pg_db_parameters)

## Tablas de entrada

#############
#### 422 ####
#############

nodos_demanda_422 = PostgisToDf(con, "nodos_demanda")
paraderos_422 = PostgisToDf(con, "paraderos")
dij_422 = PostgisToDf(con, "dij")

## Dataframe dij a matriz
dij_matrix_422 = DfToMatrix(dij_422)

## Se agrupan las entradas de interés en una única instancia
instancia_422 = MakeInstance(dij_matrix_422, nodos_demanda_422$ai, nodos_demanda_422$ni, paraderos_422$wj)

## Se genera la solución inicial
xj_ini_422 = GenerateInitialSolution(paraderos_422, 5)

#opcional: Se transforma instancia en archivo DAT
InstanceToDat(instancia, "instancia422")
InstanceToDat(instancia_g08, "instancia_g08")
InstanceToDat(instancia_i09, "instancia_i09")
InstanceToDat(instancia_c01, "instancia_c01")


#opcional: Se lee la instancia
instancia = DatToInstance("instancia422.dat", 526, 99)


instancia_test = DatToInstance("instancia_i09.dat", 415, 49)

### Resultados ####

#### Simulated Annealing ####

mejores_resultados = SimulatedAnnealing(instancia = instancia_422,
                                        xj_ini = xj_ini_422, 
                                        operador = "swap", 
                                        max_iter = 635,
                                        max_iter_interna = 25, 
                                        alpha = 0.6)

resultados_sa_split = SimulatedAnnealing(instancia, xj_ini, "swap_split", 200, 6, 0.5)

#############
#### G08 ####
#############

nodos_demanda_g08 = PostgisToDf(con, "nodos_demanda_g08")
paraderos_g08 = PostgisToDf(con, "paraderos_g08")
dij_g08 = PostgisToDf(con, "dij_g08")

## Dataframe dij a matriz

dij_matrix_g08 = DfToMatrix(dij_g08)

## Se agrupan las entradas de interés en una única instancia
instancia_g08 = MakeInstance(dij_matrix_g08, nodos_demanda_g08$ai, nodos_demanda_g08$ni, paraderos_g08$wj)

## Se genera la solución inicial
xj_ini_g08 = GenerateInitialSolution(paraderos_g08, 58)


#############
#### I09 ####
#############

nodos_demanda_i09 = PostgisToDf(con, "nodos_demanda_i09")
paraderos_i09 = PostgisToDf(con, "paraderos_i09")
dij_i09 = PostgisToDf(con, "dij_i09")

## Dataframe dij a matriz

dij_matrix_i09 = DfToMatrix(dij_i09)

## Se agrupan las entradas de interés en una única instancia
instancia_i09 = MakeInstance(dij_matrix_i09, nodos_demanda_i09$ai, nodos_demanda_i09$ni, paraderos_i09$wj)



#############
#### C01 ####
#############

nodos_demanda_c01 = PostgisToDf(con, "nodos_demanda_c01")
paraderos_c01 = PostgisToDf(con, "paraderos_c01")
dij_c01 = PostgisToDf(con, "dij_c01")

## Dataframe dij a matriz

dij_matrix_c01 = DfToMatrix(dij_c01)

## Se agrupan las entradas de interés en una única instancia
instancia_c01 = MakeInstance(dij_matrix_c01, nodos_demanda_c01$ai, nodos_demanda_c01$ni, paraderos_c01$wj)

## Se genera la solución inicial
xj_ini_g08 = GenerateInitialSolution(paraderos_g08, 58)



### Resultados ####

#### Simulated Annealing ####

mejores_resultados_g08 = SimulatedAnnealing(instancia_g08, xj_ini_g08, "swap", 635, 25, 0.6)

resultados_sa_split = SimulatedAnnealing(instancia_422, xj_ini_422, "swap_split", 200, 6, 0.5)



## Resultados Iteraciones

eval_iter = IterateSimulatedAnnealing(instancia, 
                                      n_iteraciones = 15, 
                                      operador = "swap_split", 
                                      max_iter = 635, 
                                      max_iter_interna = 25, 
                                      alpha = 0.6)

PlotSIC(eval_iter, "swap_split")



################
## RESULTADOS ##
################

# Se calcula el SIC 
xj_base = GenerateInitialSolution(instancia_i09, 0)
max_sic = EvaluateSIC(instancia_i09, xj_base)



##############################
##### Simulated Annealing ####
##############################

## Se itera 11 veces el algoritmo S.A y se almacenan sus resultados.


resultados_sa = IterateSimulatedAnnealing(instancia = instancia_i09,
                          n_iteraciones = 11, 
                          n_paraderos = 6, 
                          operador = "swap",
                          max_iter = 593, 
                          max_iter_interna = 50, 
                          alpha = 0.93)

## Se obtiene el tiempo de ejecución de cada iteración y el máximo SIC encontrado
sa_time_sic = GetSICAndTimeList(resultados_sa, "SA")


## Se grafican los resultados de todas las iteraciones
PlotSIC(resultados_sa, "swap")


## Se asocia el resultado a el shape de salida
paraderos_i09$config_sa = resultados_sa$iter4$xj


## Se extrae cómo shape
#output
st_write(obj = paraderos_i09, "DATOS/SHP/paraderos_i09.shp")

############################
##### Genetic Algorithm ####
############################

## MEJOR SOLUCIÓN POSIBLE ##

# Se genera vector que contenga todas las posibles soluciones para localizar 40 de 45 paraderos
posibles_soluciones_i09 = GenerarPosiblesSoluciones(45, 40)

# Se obtiene el máximo SIC posible para las soluciones generadas
max_sic = ObtenerMayorSIC(posibles_soluciones_i09, instancia_i09)


## Se ejecuta el G.A 11 veces con los parámetros obtenidos por Irace
resultados_ga = IterateGeneticAlgorithm(instancia = instancia_i09, 
                                        n_iteraciones = 2,
                                        n_miembros = 40,
                                        operador = "crossover_manuel",
                                        n_paraderos = 6,
                                        max_iter = 273, 
                                        prob_mutacion = 0.73)

# Se obtienen los resultados de SIC, tiempo de ejecución e hijos de 
ga_time_sic = GetSICAndTimeList(resultados_ga, instancia_i09, "GA")

# Se genera un DF con los resultados del SIC para el último hijo 
ga_df = ListsToDataFrame(ga_time_sic, max_sic)

# Se grafican los resultados en un boxplot
HijosABoxplot(ga_df)

## Gráfico convergencia

## Seleccionar mejor iteración
plot(max_sic - (resultados_ga$iter1$evol), type = "l", col = "#63B389", lwd = 2,
     main = "Gráfico Convergencia Mejor Iteración G.A",
     xlab = "N° iteración",
     ylab = "Diferencia con S.I original")


#################
## COMPARACIÓN ##
#################

# Se generan DF's de los resultados de ambos algoritmos
df_bar_sa = ListaResultadoADfPlot(sa_time_sic, "SA")
df_bar_ga = ListaResultadoADfPlot(ga_time_sic, "GA")

# Se unen ambos df's
df_bar = rbind(df_bar_sa, df_bar_ga)


# Gráfico para Variación de SIC
ggplot(df_bar, aes(fill=algoritmo, y=max_sic - sic, x=iter)) +
  geom_bar(position='dodge', stat='identity') +
  ggtitle("Diferencia SIC por iteración") +
  xlab("N° Iteración") +
  ylab("Diferencia SIC") +
  scale_fill_manual('Metaheurística', values=c('#ffba4c','#63B389'))


# Gráfico para Variación de tiempo de ejecución
ggplot(df_bar, aes(fill=algoritmo, y=time, x=iter)) +
  geom_bar(position='dodge', stat='identity') +
  ggtitle("Tiempo ejecución por iteración") +
  xlab("N° Iteración") +
  ylab("Tiempo Ejecución (min)") +
  scale_fill_manual('Metaheurística', values=c('#ffba4c','#63B389'))



###############











plot = plot((max_sic - resultados_sa$eval_si), type = "l", col = "#63B389", lwd = 2,
            main = paste(i, "paraderos\n S.I =",si_iter,"\nMínima diferencia =",dif_iter),
            xlab = "N° iteración",
            ylab = "Diferencia con S.I original")

dev.off()




jpeg(paste("DATOS/JPEG/sic_sa_",i,"_paraderos.jpg",sep = ""), width = 1000, height = 700)

plot = plot((max_sic - resultados_sa$eval_si), type = "l", col = "#63B389", lwd = 2,
            main = paste(i, "paraderos\n S.I =",si_iter,"\nMínima diferencia =",dif_iter),
            xlab = "N° iteración",
            ylab = "Diferencia con S.I original")

dev.off()



plot((resultados_sa_swap$eval_si), type = "l", col = "#63B389", lwd = 2,
     main = paste(i, "paraderos\n S.I =",si_iter,"\nMínima diferencia =",dif_iter),
     xlab = "N° iteración",
     ylab = "Diferencia con S.I original")









