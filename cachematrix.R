## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix loads a matrix as an object into the cache

makeCacheMatrix <- function(x=matrix()){
  invert <- NULL
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {invert <<- inverse}
  getInverse <- function() {invert}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## cacheSolve returns an inverted matrix of 'x', and if it has already been returned and not changed, it returns the same value with a message stating it's from the cache

cacheSolve <- function(x,...){
  invert <- x$getInverse()
  if(!is.null(invert)){
    message("Retrieving data from cache")
    return(invert)
  }
  mat <- x$get()
  invert <- solve(mat,...)
  x$setInverse(invert)
  invert
}
