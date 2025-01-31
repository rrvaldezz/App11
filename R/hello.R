#Paquete que genera un menu
#para matrices
#por RONNY VALDEZ
#fecha: 31-01-2025
#menu para ingrreso de numero de filas
#numero de columnas con funciones
#cuantos numeros pares e impares tiene
#cuales son los numeros pares dentro del vector

crear_matriz <- function(nf,nc){
  m1 <- matrix(NA, nrow = nf, ncol = nc)
  return(m1)
}
#numero de filas y columnas
ingreso_param <-function(){
  cat("Ingrese el número de filas", "\n")
  nf <- readLines(n=1)
  nf <- as.numeric(nf)
  cat("Ingrese el número de columnas", "\n")
  nc <- readLines(n=1)
  nc <- as.numeric(nc)
  return(list(nf=nf, nc=nc))
}

#validar y mostrar la matriz
val_matriz <- function(m1,nf,nc){
  for(i in 1:nf){
    for(j in 1:nc){
      cat("Ingrese el elemento [", i, ",", j,"]: \n")
      m1[i,j] <- readLines(n=1)
      m1[i,j] <- as.numeric(m1[i,j])
    }
  }
  return(m1)
}
cant_par_impar <- function(m1,nf,nc){
  p=0
  imp=0
  for (i in 1:nf){
    for (j in 1:nc) {
      if(m1[i,j]%%2==0){
        p <- p+1
    }else{
      imp <- imp+1
    }
  }
}#termina el for
return(list(p=p, imp=imp))
}

val_par_vect <- function(m1,nf.nc){
  v1 <- c()
  v2 <- c()
  k <- 1
  q <- 1
  for(i in 1:nf){
    for(j in 1:nc){
      if(m1[i,j]%%2==0){
        v1[k] <- m1[i,j]
        k <- 1
    }else{
      v2[q] <- m1[i,j]
        q <- q+1
    }
  }
  }
  print(v1)#
  print(v2)#
#return(list(v1,v2))
}

menu <- function(){
  cat("MENÚ PRINCIPAL \n")
  cat("Opción 1. Ingreso de Matriz  \n")
  cat("Opción 2. Cuántos pares e impares tiene la Matriz  \n")
  cat("Opción 3. Valores pares e impares  \n")
  cat("Opción 4. Salir  \n")
  cat("Escoja una opción:  \n")
  Op1 <- readLines(n=1)
  Op2 <- as.numeric(Op1)
  return(Op1)
}

menu1 <- function(){
  F <- ingreso_param()
  nf1 <- F$nf
  nc1 <- F$nc
  m <- crear_matriz(nf1,nc1)
  mat1 <- val_matriz(m,nf1,nc1)
  #cambio a numéricos los valores de la matriz
  mat2 <- matrix(as.numeric(mat1), nrow = nrow(mat1), ncol = ncol(mat1))
  ban=0
  while(ban==0){
    Op1 <- menu()
  switch(Op1,
        "1" = {
          cat("Visualizar la matriz ingresada  \n")
          print(mat1)
          print(mat2)

        },
        "2" = {
          cat("Existen en la matriz \n")
          c1 <- cant_par_impar(mat2,nf1,nc1)
          cat("Pares=", c1$p, "\n")
          cat("Impares=", c1$imp, "\n")
        },
        "3"= {
          cat("Valores pares e impares de la matriz  \n")
          l1vect <- val_par_vect(mat2,nf1,nc1)
          cat("Vector 1 con pares \n")
          v1 <- c()
          v1 <- l1vect$v1
          print(l1vect$v1)
          cat("Vector 2 con impares \n")
          v2 <- c()
          v2 <- l1vect$v2
          print(l1vect$v2)
        },
        "4"= {
          cat("Saliendo del sistema...")
          ban=1
        },
        {
          cat("Error, digite bien la opción  \n")
        }
  )
  }#cierre while
}
