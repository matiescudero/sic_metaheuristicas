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


swap<-function(sol,i,j){
  piv<-sol[i]
  sol[i]<-sol[j]
  sol[j]<-piv
  return(sol)
}


evaluate_SIC = function(dij, wj, ai, ni, xj){
  
  acum = 0
  n_zc = nrow(nodos_demanda)
  n_paraderos = nrow(paraderos)
  
  for(i in 1:n_zc){
    for(j in 1:n_paraderos){
      sij = ((wj[j]**2 * dij[i,j]**-2)/(ni[i]))*ai[i]*xj[j]
      acum = acum + sij
      
    }
  }
  return(acum)
}



evaluate_SIC(dij_matrix, wj, ai, ni, xj)


xj = rep(1, 99)

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

## Demanda
ai = nodos_demanda$ai
ni = nodos_demanda$ni

## Paraderos
wj = paraderos$wj

# SIMULATED ANNEALING

## Se genera la solución inicial
xj = generate_initial_solution(paraderos, 90)


