library(RPostgres)
library(DBI)
library(sf)
library(tidyr)
library(textshape)

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


# Generar solución inicial aleatoria

GenerateInitialSolution = function(df_paraderos, p){
#' Genera el vector Xj inicial de forma aleatoria, indicando el n° p de paraderos a seleccionar.
#' Además, se incluye el nombre de cada paradero en el vector generado. 
#'
#' @param df_paraderos (data.frame) Dataframe que contiene los paraderos a seleccionar
#' @param p (int) Número de paraderos a localizar. Debe ser igual o menor a n.
#'
#' @return xj (array) Vector de solución generado aleatoriamente
  
  # Número de paraderos disponibles
  n = nrow(df_paraderos)
  
  # Vector de solución aleatoria
  xj = c(rep(0, n - p), rep(1, p))
  xj = sample(xj)
  
  # Se indica el id de cada paradero para el vector generado
  names(xj) = df_paraderos$stop_id
  
  
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

SimulatedAnnealing = function(instancia, xj_ini, operador, max_iter, iter_temp, alpha){
  #' Calcula el menor costo al aplicar el algoritmo de S.A a una función objetivo dada.
  #' 
  #' @param instancia (list) Lista que incluye la matriz dij y los vectores ai, ni y wj
  #' @param xj_ini (array) Vector que contiene una configuración inicial de Xj para el modelo SIC
  #' @param operador (character) Nombre del operador a aplicar. Puede ser "swap" o "swap_split"
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
  i = 1
  i_ciclo = 1
  n = length(xj)
  
  ## Ciclo while
  while(i < max_iter){
    
    t_iter = 1
    
    while (t_iter < iter_temp){
      
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
      
      t_iter = t_iter + 1
      }
    
    # Se aplica un factor de enfriamiento de alpha (parámetro de la función)
    t = t*alpha
    
    eval_iter = c(eval_iter, i_ciclo)
    eval_si_iter = c(eval_si_iter, spatial_interaction)
    i_ciclo = i_ciclo + 1
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



#### MAIN ####

### ENTRADAS ###

## Conexión a BD

con = ConnectionToDb(pg_db_parameters)

## Tablas de entrada

nodos_demanda = PostgisToDf(con, "nodos_demanda")
paraderos = PostgisToDf(con, "paraderos")
dij = PostgisToDf(con, "dij")

### PROCESAMIENTO ###

## Dataframe dij a matriz

dij_matrix = DfToMatrix(dij)

## Se agrupan las entradas de interés en una única instancia
instancia = MakeInstance(dij_matrix, nodos_demanda$ai, nodos_demanda$ni, paraderos$wj)

## Se genera la solución inicial
xj_ini = GenerateInitialSolution(paraderos, 90)

### Resultados ####

resultados_sa_swap = SimulatedAnnealing(instancia, xj_ini, "swap", 400, 10, 0.5)

resultados_sa_split = SimulatedAnnealing(instancia, xj_ini, "swap_split", 400, 10, 0.5)

##

resultados_sa_swap$spatial_interaction
resultados_sa_split$spatial_interaction

jpeg("rplot.jpg", width = 900, height = 500)

plot((resultados_sa_split$eval_si), type = "l", col = "red",lwd = 2, 
     main = "Simulated Annealing\n Modelo SIC",
     xlab = "N° iteración",
     ylab = "Spatial Interaction")

dev.off()

## Plot para iteraciones más pequeño
plot(resultados_sa$eval_si_iter, type = "l", col = "red",
     sub = "temp = 10",
     main = "Simulated Annealing\n Modelo SIC",
     xlab = "N° iteración",
     ylab = "Spatial Interaction")


## Nuevos resultados
paraderos$xj = resultados_sa$xj


#output
st_write(obj = paraderos, 
         "DATOS/SHP/paraderos_sa.shp")






## TEST

xj_base = generate_initial_solution(paraderos, 99)

max_sic = evaluate_SIC(instancia, xj_base)


# Tracking
eval_iter = numeric()
eval_si = numeric()
eval_xj = numeric()
eval_time = numeric()
eval_plot = numeric()
eval_dif = numeric()

for (i in 85:98){
  
  #reloj
  start_time = Sys.time()
  
  xj_ini = generate_initial_solution(paraderos, i)
  
  resultados_sa = simulated_annealing(instancia, xj_ini, 300, 10, 0.9)
  
  end_time = Sys.time()
  
  time_iter = as.numeric(end_time - start_time)
  
  xj_iter = resultados_sa$xj
  si_iter = resultados_sa$spatial_interaction
  dif_iter = round(max_sic - si_iter,2)
  
  jpeg(paste("DATOS/JPEG/sic_sa_",i,"_paraderos.jpg",sep = ""), width = 1000, height = 700)
  
  plot = plot((max_sic - resultados_sa$eval_si), type = "l", col = "#63B389", lwd = 2,
              main = paste(i, "paraderos\n S.I =",si_iter,"\nMínima diferencia =",dif_iter),
              xlab = "N° iteración",
              ylab = "Diferencia con S.I original")
  
  dev.off()
  
  eval_iter = c(eval_iter, i)
  eval_si = c(eval_si, si_iter)
  eval_xj = c(eval_xj, xj_iter)
  eval_time = c(eval_time, time_iter)
  eval_plot = c(eval_plot, plot)
  
  
  print(paste("iteración",i,"lista. Tiempo de ejecución:",time_iter,"seg"))
  
}



xj_p93 = eval_xj[(1+(99*8)):(99+(99*8))]
xj_p94 = eval_xj[(1+(99*9)):(99+(99*9))]
xj_p95 = eval_xj[(1+(99*10)):(99+(99*10))]


## Nuevos resultados
paraderos$xj_p93 = xj_p93
paraderos$xj_p94 = xj_p94
paraderos$xj_p95 = xj_p95


#output
st_write(obj = paraderos, 
         "DATOS/SHP/paraderos_sa.shp")



xj_ini = GenerateInitialSolution(paraderos, 90)











names(xj_list$zeros[paradero_zero]) = paradero_one

sample(names(xj_zeros),1)

xj_zeros[["PJ54"]]
