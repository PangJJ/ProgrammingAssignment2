## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix --> a wrapper object that holds the matrix and its inverse value
## cacheSolve      --> a wrapper for solve(); searches the makeCacheMatrix wrapper
##                     provided for its inverse, returns if it exists else the
##                     inverse will be calculated and stored
##
## Cache is based on per object, .i.e. if another matrix object (of exact values as
## one of the matrix objects in the wrapper) requests for an inverse, the inverse will
## be calculated even though another matrix of the exact values has already computed
## the inverse. Might be better if the (key,value) pair of (matrix,inverse) are stored 
## in a central-list(external scope to the function) instead of a single wrapper object.
##
## ...
##
## Or if we can maintain a persistent datastructure such as list/vector in 
## makeCacheMatrix, we can "grow" the datastructure so it'll contain the list of 
## (matrix,inverse) value pair


## Write a short comment describing this function
## Wrapper function storing the matrix and its inverse
## Getter and setter methods included
makeCacheMatrix <- function(matrice = matrix()) {
  inverse <- NULL
  set <- function(mat){
    matrice <<- y
    inverse <<- NULL
  }
  get <- function() matrice
  setInverse <- function(inv_matrix) inverse <<- inv_matrix
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## Searches the wrapper object (makeCacheMatrix) for cached inverse
## If it does not exist, the inverse will be calculated and cached
cacheSolve <- function(x, ...) {
  matrix_inv <- x$getInverse()
  if(!is.null(matrix_inv)){
    message("getting cached data")
    return(matrix_inv)
  }
  data <- x$get()
  matrix_inv <- solve(data, ...)
  x$setInverse(matrix_inv)
  matrix_inv
}
