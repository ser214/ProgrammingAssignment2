## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # set matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse
  }
  
  # value of matrix
  get <- function() x
  
  # cached inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the CacheMatrix and if the inverse is already calculated, returns it.

cacheSolve <- function(x, ...) {
  
  # Check if the inverse is already cached
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Get the matrix from the special "matrix" object
  data <- x$get()
  
  # Compute the inverse of the matrix
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  inv
}
