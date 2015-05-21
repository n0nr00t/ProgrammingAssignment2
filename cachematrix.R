# Escribir una funcion para crear la cache de una matrix Inversa
#
# by @nonroot , 2015
#
# Que tambien funciona?, miremos los tiempos con y sin cache.
# > x <- diag(5000)
# > m <- makeCacheMatrix(x)
# > system.time(cacheSolve(m))
# user  system elapsed 
# 62.182   0.704  64.535 
# > system.time(cacheSolve(m))
# user  system elapsed 
# 0       0       0 
# > system.time(cacheSolve(m))
# user  system elapsed 
# 0       0       0 
# > 
  
#Crear una lista que puede almacenar la matrix "especial"
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInversa <- function(mInversa) m <<- mInversa
  getInversa <- function() m
  list(set = set, get = get,
       setInversa = setInversa,
       getInversa = getInversa)
}

# Obtenga la inversa de la matrix, si la matrix de entrada esta nula, calculela
# Esto solo se hace la primera vez, si no es nula, entonces obtenga la cache
cacheSolve <- function(x, ...) {
  mInversa <- x$getInversa()
  if(!is.null(mInversa)) {
    return(mInversa)
  }
  matrix <- x$get()
  mInversa <- solve(matrix)
  x$setInversa(mInversa)
  mInversa
}
