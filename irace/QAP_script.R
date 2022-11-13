# #======================
# #Parámetros de entrada
# #======================
# entrada= "kra30b.dat"
# N = 50 # Iteraciones internas
# parada = 555 # Criterio de parada
# T=50000
# alpha=0.7
# operador=1 #Define 1 de 3 operadores de permutación a usar


#======================================================================================
# B) Definición de funciones
#======================================================================================
simmulated_annealing=function(entrada,N,parada,T,alpha,operador)
{
    #-------------------------------------------------------------------------------------
    # leer_instancia() - Hace la lectura de datos (Implementado por Mario Inostroza)
    # Entrada = nombre de instancia / Salida= datos con matrices f y f
    #-------------------------------------------------------------------------------------
    leer_instancia=function(nombre){ 
      
      nombre = paste("Instancias/",nombre,sep="")
      print(getwd())
      
      a =read.delim(nombre,header=FALSE, sep ="")

      
      #Obtener el puntaje máximo
      best= as.integer(a[1,1])
      
      #Obtener el número de filas y columnas
      n = as.integer(a[2,1])
      
      #Remover ambos elemenos
      a=a[c(-1,-2),]
      
      #Obtener fl
      fl=a[1:round(nrow(a)/2),]
      fl=matrix(unlist(fl),n,n)
      
      
      #Obtener dist
      dis=a[(round(nrow(a)/2)+1):nrow(a),]
      dis= matrix(unlist(dis),n,n)
      
      d = list(n=n, best= best, f= fl, d = dis)
      return(d)
    }
    
    #-------------------------------------------------------------------------------------
    # evaluarQAP(datos,solución) - Reliza la evaluación de una solución
    # Entradas = Matriz D y posiciones / Salida= función de fitness (Error cuadrático)
    #-------------------------------------------------------------------------------------
    #Función de evaluación
    evaluarQAP=function(datos,solucion){
      datos$d=datos$d[solucion,solucion]
      puntaje=sum(datos$f*datos$d)
      return(puntaje)
    }
    
    
    #-------------------------------------------------------------------------------------
    # per_intercambio(solucion) - Realiza una perturbación basada en intercambio de posiciones
    # Entrada = solucion / Salida= indices o solución modificada
    #-------------------------------------------------------------------------------------
    per_intercambio=function(solucion)
    { a=sample(solucion,1,replace = F)
      b=sample(solucion,1,replace = F)
      indices=solucion
      indices[a]=solucion[b]
      indices[b]=solucion[a]
      return(indices)
    }
    
    #-------------------------------------------------------------------------------------
    # per_insersión(solucion) - Realiza una perturbación basada en la inserción de una posición
    # Entrada = solucion / Salida= indices o solución modificada
    #-------------------------------------------------------------------------------------
    per_insercion=function(solucion)
    {  
      a=sample(solucion,1,replace = F)
      b=sample(solucion,1,replace = F)
      indices=solucion
      indices=indices[-a] #Elimina el valor de la posición a
      indices=append(indices,solucion[a],(b-1)) # lo agrega en otra posición
      return(indices)
    }
    
    #-------------------------------------------------------------------------------------
    # per_inversion(solucion) - Realiza una perturbación basada en la inversión entre dos posiciones
    # Entrada = solucion / Salida= indices o solución modificada
    #-------------------------------------------------------------------------------------
    per_inversion=function(solucion)
    {
      a=sample(solucion,1,replace = F)
      b=sample(solucion,1,replace = F)
      solucion[a:b]=solucion[rev(sort(seq(a,b)))] #Invierte
      return(solucion) 
    }
    
    
    
    #======================================================================================
    # C) Simmulated Annealing
    #======================================================================================
    datos=leer_instancia(entrada)
    solucion_sa=1:ncol(datos$d)
    solucion_best_sa=solucion_sa
    
    print(evaluarQAP(datos,solucion_sa))
    
    resultados=NULL
    
    for (a in 1:parada)
    {
      
      for (b in 1:N)
      {
        #Perturbación
        if (operador==1){solucion_tmp=per_intercambio(solucion_sa)}
        if (operador==2){solucion_tmp=per_insercion(solucion_sa)}
        if (operador==3){solucion_tmp=per_inversion(solucion_sa)}
        
        #Reemplazo de solución
        deltaE=evaluarQAP(datos,solucion_tmp)-evaluarQAP(datos,solucion_sa)
        
        #Evaluación
        #Mejor de todas
        if (evaluarQAP(datos,solucion_tmp)<evaluarQAP(datos,solucion_best_sa))
        {solucion_best_sa=solucion_tmp}
        
        #Si la s' es mejor que s
        if (deltaE<=0){solucion_sa=solucion_tmp} else
          #Si s es mejor que s'
        { prob=exp(-deltaE/T)
        prob_acep=runif(1)
        if (prob_acep<prob) {solucion_sa=solucion_tmp} #Se acepta s'
        }
        
        resultados=rbind(resultados,c(evaluarQAP(datos,solucion_best_sa),evaluarQAP(datos,solucion_sa)))
        
        
      } # Fin de condición de equilibrio
      
      T = T * alpha
      
    }
    
    
    resultados=cbind(1:nrow(resultados),resultados)
    resultados=data.frame(resultados)
    colnames(resultados)=c("ITE","BEST","SCORE")
    print(tail(resultados))
    
    print(paste("El mejor resultado es:",datos$best,sep=""))
    return(resultados[nrow(resultados),3])
    #return(resultados)
}

#resultados=simmulated_annealing(entrada,N,parada,T,alpha,operador)
#g = ggplot(resultados, aes(ITE))
#g = g + geom_point(aes(y=BEST), colour="darkblue") + geom_line(aes(y=BEST),size=1) +ylab("Score")
#g = g + geom_line(aes(y=SCORE), colour="lightskyblue")  + theme_classic()
#plot(g)
