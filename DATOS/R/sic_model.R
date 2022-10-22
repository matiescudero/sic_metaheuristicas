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

postgis_to_df = function(connection, layer){
  df = st_read(connection, layer)
  return(df)
}

# Transformar dataframe de distancias a matriz

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
#' @param instancia (list) Lista que in
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


# Generar conexión

con = connect_to_db(dvr, db, host, port, user, passwd)

# ENTRADAS

nodos_demanda = postgis_to_df(con, "nodos_demanda")
paraderos = postgis_to_df(con, "paraderos")
dij = postgis_to_df(con, "dij")

# PROCESAMIENTO

# DF a Matriz

dij_matrix = df_to_matrix(dij)

# Se transforman las columnas a vectores

## Se agrupan las entradas de interés en una única instancia
instancia = make_instance(dij_matrix, nodos_demanda$ai, nodos_demanda$ni, paraderos$wj)

# SIMULATED ANNEALING

## Se genera la solución inicial
xj = generate_initial_solution(paraderos, 90)


## Evaluación SIC
spatial_interaction = evaluate_SIC(instancia, xj)

## 
initial_t = calculate_initial_temperature(instancia, xj, spatial_interaction, 0.5)


