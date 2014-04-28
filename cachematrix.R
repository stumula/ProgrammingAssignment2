## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function does the following
## set the value of a matrix to the passed in matrix
## gets the values of a matrix
## sets the inverse of a matrix
## gets the inverse of a matrix
## assumption is the passed in matrix can be inverted
##
## usage is to set the value of matrix variable then call makeCacheMatrix to
## be passed to cacheSolve

makeCacheMatrix <- function( x = matrix() ) {
  m <- NULL
  
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set,get = get,setInverse=setInverse,getInverse=getInverse)
}

## call this function to cache the inverse of a matrix
## pass in the "matrix" returned by makeCacheMatrix

cacheSolve <- function(x = matrix(), ...){
  m <- x$getInverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}