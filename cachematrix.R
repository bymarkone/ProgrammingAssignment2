## makeCacheMatrix creates a wrapper for a matrix that stores the matrix data 
## and potentialy its inverse. cacheSolve first look for the cached inverse within 
## the cached matrix, otherwise it computes and stores the inverse

## a function the stores a matrix and caches its inverse 

makeCacheMatrix <- function(aMatrix = matrix()) {
  inverse <- NULL
  set <- function(newMatrix) {
    aMatrix <<- newMatrix
    inverse <<- NULL
  }
  get <- function() aMatrix
  setinverse <- function(newInverse) inverse <<- newInverse
  getinverse <- function() inverse
  list (set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## Return the cached matrix inverse of compute (and store) one if not cached 

cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getinverse()
  if (!is.null(inverse)) {
    return(inverse)
  } 
  data <- cacheMatrix$get()
  inverse <- solve(data)
  cacheMatrix$setinverse(inverse)
  inverse
}
