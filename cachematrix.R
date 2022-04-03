## Put comments here that give an overall description of what your
## functions do

## : This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getMatrix <- function() x
  settingInverse <- function(inverse) i <<- inverse
  gettingInverse <- function() i
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       settingInverse = settingInverse,
       gettingInverse = gettingInverse)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$gettingInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  dataMatrix <- x$getMatrix()
  i <- solve(dataMatrix, ...)
  x$settingInverse(i)
  i
}
