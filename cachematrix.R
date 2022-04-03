## Put comments here that give an overall description of what your
## functions do

## : This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##This function takes matrix as the argument.
  
  ##Assign iMatrix to NULL 
  iMatrix <- NULL
  ##Set Matrix 
  setMatrix <- function(yData) {
    x <<- yData
    iMatrix <<- NULL
  }
  getMatrix <- function() x
  
  settingInverse <- function(inverseMatrix) iMatrix <<- inverseMatrix
  
  gettingInverse <- function() iMatrix

  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       settingInverse = settingInverse,
       gettingInverse = gettingInverse)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(dMat, ...) {
  z <- dMat$gettingInverse()
  if (!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  dataMatrix <- dMat$getMatrix()
  z <- solve(dataMatrix, ...)
  dMat$settingInverse(z)
  z
}
