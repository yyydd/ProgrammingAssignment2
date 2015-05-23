## This document contains two functions, namely makeCacheMatrix and cacheSolve.


## MakeCacheMatrix fuction takes a matrix as a argument and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ##set function changes the matrix stored and assigns null to the variable which stores its inverse i.e.'i'
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  
  ##get function fetches the value of the matrix stored
  get <- function() x
  
  
  ##setInverse function sets i to the inverse of the matrix
  setInverse <- function(inverse) i <<- inverse
  
  
  ##GetInverse function fetches the inverse of the matrix (which is stored in 'i')
  getInverse <- function() i
  
  
  ##makeCacheMatrix returns a function list containing the following four functions 
  ## 1)set function, 2)get function, 3)setInverse function and 4)getInverse function
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## CacheSolve function calculates the inverse of the object either by fetching it from the cache 
## or by using Solve function if the matrix has changed.

##Important note- This function assumes that the given matrix is invertible.

cacheSolve <- function(x, ...) {
  
  ##tries to get the cached inverse
  i <- x$getInverse()
  
  ##if cached inverse is available, the function returns the cached inverse
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ##if the cached inverse is not available, the value of the matrix is fetched and stored in 'data'
  data <- x$get()
  
  ##inverse of data is calculated and stored in 'i'
  i <- solve(data, ...)
  
  ##the inverse is set to be equal to i
  x$setInverse(i)
  
  ##the inverse of the matrix is returned
  i
}
