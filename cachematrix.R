## The following functions are used to cache the inverse of a matrix
 

## makeCacheMatrix creates and returns a list of functions to set or
## get the inserse of a matrix in cache:

## set: sets the value of the matrix
## get: gets the value of the matrix
## setinverse: sets the value of inverse of the matrix
## getinverse: gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix from cache 
## if it exists else it calculates and returns the inverse 
## of the matrix and stores it in cache 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
