## This pair of functions cache the inverse of a matrix. 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
# Setting and getting the matrix.
# setting and getting the inverse of theh matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# Returning the inverse of the matrix. If the inverse has already been calculated the cachesolve should skip the computation and retrieve the inverse from the cache
 # We will assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Sample run:

x <- matrix(c(1,5,15,20), 2, 2)
m = makeCacheMatrix(x)
m$get()   # Get x without inversion
cacheSolve(m) # x inverion
