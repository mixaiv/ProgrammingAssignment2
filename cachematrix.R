## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## raw for inverted matrix. 
## 'set' function for setting the value for x.
## 'get' function for getting our matrix x. 
## 'setInvertedMatrix' function for storing inverted matrix
## 'getInvertedMatrix' function for obtaining cached inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  raw <- NULL
  set <- function(param){
    x <<- param
    raw <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setInvertedMatrix <- function(invertedMatrix){
    raw <<- invertedMatrix
  }
  
  getInvertedMatrix <- function(){
    raw
  }
  
  data <- list(set = set, get = get, setInvertedMatrix = setInvertedMatrix, getInvertedMatrix = getInvertedMatrix)
}


## Write a short comment describing this function
## First checking if there is a inverted matrix cached already.
## If there is not, inverting matrix. Before inverting check the necessary conditions to be fulfilled.
## After obtaining inverted matrix cache it.
## Return the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matr <- x$getInvertedMatrix()
  if (!is.null(matr)){
    message("Get cached inverted matrix")
    return(matr)
  }
  
  raw <- x$get()
  ## Check our matrix for necessary conditions before inverting it
  ## Our matrix 'x' has to be square matrix. And our matrix 'x' determinant must not be zero.
  if (dim(raw)[1] == dim(raw)[2] & det(raw) != 0){
    matr <- solve(raw, ...)
  }
  x$setInvertedMatrix(matr)
  matr
}
