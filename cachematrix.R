## The goal of the following functions is to create
## a process that can take the inverse of a matrix 
## and cache it for future use since taking the
## inverse of a matrix can be a costly operation

## The first function, makeCacheMatrix,
## will create a matrix object that can
## be used to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setInverse <- function(cache) cache <<- solve(x)
  getInverse <- function() cache
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The second function, cacheSolve, will
## first check if the inverse of the given
## matrix already exists in the object created
## by makeCacheMatrix, if it's not it will
## calculate the inverse of the matrix
## and then cache it

cacheSolve <- function(x, ...) {
  cache <- x$getInverse()
  if(!is.null(cache) && x$getInverse() == x$setInverse()) {
    message("Getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setInverse(cache)
  cache
}
