library(RPostgres)
library(DBI)
library(sf)
library(tidyr)
library(textshape)

#### Credenciales DB ####

dvr = RPostgres::Postgres()
db = 'metaheuristicas'  
host = 'localhost'
port = '5432' 
user = 'postgres'  
passwd = 'postgres'


####################
#### FUNCIONES #####
####################

## BD a R ##

# Genera Conexión a BD

connect_to_db = function(dvr, db, host, port, user, passwd){
  con = dbConnect(dvr, db, host, port, user, passwd, options="-c search_path=output")
  return(con)
}

# Almacenar tablas espaciales

postgis_to__rownames(dij_matrix, "zc_id")
  dij_matrix = as.matrix(dij_matrix)
  return(dij_matrix)df = function(connection, layer){
  df = st_read(connection, layer)
}

# Transformar dataframe de distancias a matriz  dij_matrix = column_to


df_to_matrix = function(distance_df){
  dij_matrix = spread(distance_df, stop_id, distance)
  dij_matrix = column_to_rownames(dij_matrix, "zc_id")
  dij_matrix = as.matrix(dij_matrix)
  return(dij_matrix)
}

# Generar instancia
make_instance = function(dij, ai, ni, wj){
#'Agrupa las entradas en una lista
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


# Generar solución inicial aleatoria

generate_initial_solution = function(df_paraderos, p){
#'Genera el vector Xj inicial de forma aleatoria, indicando el n° p de paraderos a seleccionar 
#'
#'@param df_paraderos (data.frame) Dataframe que contiene los paraderos a seleccionar
#'@param p (int) Número de paraderos a localizar. Debe ser igual o menor a n.
#'
#'@return xj (array) Vector de solución generado aleatoriamente
  
  # Número de paraderos disponibles
  n = nrow(df_paraderos)
  
  # Vector de solución aleatoria
  xj = c(rep(0, n - p), rep(1, p))
  xj = sample(xj)
  
  return(xj)
}


get_swap_numbers = function(xj){
  
#' Obtiene las posiciones para realizar el operador swap. Si al evaluar estas posiciones dentro del
#' vector xj son idénticas se obtendrán nuevos números hasta que dejen de ser iguales.
  
  n = length(xj)
  is_equal = TRUE
  
  while(is_equal){
    
    n_swap = sample(2:n, 2)
    
    if(xj[n_swap[1]] != xj[n_swap[2]]){
      is_equal = FALSE
    }
  }
  
  return(n_swap)
}


swap<-function(sol,i,j){
  
  piv<-sol[i]
  sol[i]<-sol[j]
  sol[j]<-piv
  return(sol)
}


evaluate_SIC = function(instancia, xj){
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
  n_zc = length(ai)
  n_paraderos = length(wj)
  
  ## Ciclo para evaluar el modelo SIC
  for(i in 1:n_zc){
    for(j in 1:n_paraderos){
      
      ## Valor de Sij para un par (i,j)
      sij = ((wj[j]**2 * dij[i,j]**-2)/(ni[i]))*ai[i]*xj[j]
      acum = acum + sij
      
    }
  }
  
  return(acum)
}


calculate_initial_temperature = function(instancia, xj, spatial_interaction, p0){

  
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
    n_swap = get_swap_numbers(xj_temp)
    xj_temp = swap(xj_temp, n_swap[1], n_swap[2])
    
    print(xj_temp)
    
    
    ## Para cada ciclo se evalúa el vector solución generado 
    nuevo_si = evaluate_SIC(instancia, xj_temp)
    print("nuevo_si:  ")
    print(nuevo_si)
    
    ## Se calcula el delta de la interacción espacial
    delta_si = nuevo_si - si_temp
    
    ## Se almacena el delta
    lista_deltas[i] = delta_si
    
    ## El SI actual pasa a ser el nuevo
    si_temp = nuevo_si
    
  }
  
  print("------lista_deltas:")
  print(lista_deltas)
  print("------")
  print(lista_si)
  
  ## Se calcula el delta promedio
  delta_promedio = mean(abs(lista_deltas))
  
  print("------")
  print(delta_promedio)
  
  ## De acuerdo a la fórmula exp(-(delta_promedio)/t_inicial) = p0, despejando queda:
  t_inicial = -delta_promedio/log(p0)
  
  return(t_inicial)
}

simulated_annealing = function(instancia, xj_ini, n_iter, alpha){
  #' Calcula el menor costo al aplicar el algoritmo de S.A a una función objetivo dada.
  #' 
  #' @param instancia (list) Lista que incluye la matriz dij y los vectores ai, ni y wj
  #' @param xj_ini (array) Vector que contiene una configuración inicial de Xj para el modelo SIC
  #' @param n_iter (int) Número de iteraciones máximas a realizar hasta que el algoritmo no entregue una mejor solución que la actual
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
  eval_mejor = numeric()
  temp = numeric()
  
  # Inicialización soluciones (xj)
  ## Se genera el vector de solución inicial 
  xj = xj_ini
  mejor_xj = xj
  
  ## Se obtiene un primer interacción espacial a partir de la evaluación de una solución random
  spatial_interaction = evaluate_SIC(instancia, xj)
  mejor_si = spatial_interaction
  
  ## Se obtiene la temperatura inicial
  t_inicial = calculate_initial_temperature(instancia, xj, spatial_interaction, 0.5)
  t = t_inicial
  
  ## Contador para ciclo loop
  i = 1
  n = length(xj)
  
  ## Ciclo while
  while(i < n_iter){
    
    #otro wail
    
    n_swap = get_swap_numbers(xj)
    xj_test = swap(xj, n_swap[1], n_swap[2])
    si_test = evaluate_SIC(instancia, xj_test)
    
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
      # Si la solución es la mejor hasta el momento, se reinicia el contador
      i = 1
    }else{
      # Si la solución del iterador es peor, se suma uno al contador y se pasa a la sig iteración,
      # El ciclo termina una vez que al iterar n_iter veces la solución no mejore.
      i = i + 1
    }
    
    # Variables de trackeo
    eval_si = c(eval_si, spatial_interaction)
    eval_mejor = c(eval_mejor, mejor_si)
    temp = c(temp, t)
    
    ##Equilibrium state?
    
    # Se aplica un factor de enfriamiento de alpha (parámetro de la función)
    t = t*alpha
  }
  
  return(list(xj = mejor_xj, 
              spatial_interaction = mejor_si, 
              eval_si=eval_si, 
              eval_mejor=eval_mejor,
              temp = temp))
}



#### MAIN ####

### ENTRADAS ###

## Conexión a BD

con = connect_to_db(dvr, db, host, port, user, passwd)

## Tablas de entrada

nodos_demanda = postgis_to_df(con, "nodos_demanda")
paraderos = postgis_to_df(con, "paraderos")
dij = postgis_to_df(con, "dij")

### PROCESAMIENTO ###

## Dataframe dij a matriz

dij_matrix = df_to_matrix(dij)

## Se agrupan las entradas de interés en una única instancia
instancia = make_instance(dij_matrix, nodos_demanda$ai, nodos_demanda$ni, paraderos$wj)

## Se genera la solución inicial
xj_ini = generate_initial_solution(paraderos, 90)

### Resultados ####

resultados_sa = simulated_annealing(instancia, xj_ini, 200, 0.5)

resultados$spatial_interaction


plot(resultados$eval_si, type = "l", col = "red", 
     main = "Simulated Annealing\n Operador SWAP",
     xlab = "N° iteración",
     ylab = "Spatial Interaction")



## Nuevos resultados
paraderos$xj = resultados$xj


#output
st_write(obj = paraderos, 
         "DATOS/SHP/paraderos_sa.shp")

