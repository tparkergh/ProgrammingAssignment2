## A collection of function for working on Matrix operations.

## Creates a vector of objects to work with a given vector.
## Takes a matrix that will be operated on by returned operations.
## Returns A vector of objects that provide an API to
## interact with matrix.
## set() sets the matrix
## get() retrieves the matrix
## getInverse() retreives the a cached inverse matrix if set, else returns null
## setInverse() sets the inverse matrix
##
## Example:
## > matrix <- matrix(c(1,3,3,1,4,3,1,3,4), nrow=3,ncol=3,byrow= TRUE)
## > matrixVector <- makeCacheMatrixVector(matrix)
## > matrixVector$get()
##         [,1] [,2] [,3]
##    [1,]    1    3    3
##    [2,]    1    4    3
##    [3,]    1    3    4
##
## > matrixVector$set(newMatrix)
##
## > matrixVector$getInverse()
##          [,1] [,2] [,3]
##    [1,]    7   -3   -3
##    [2,]   -1    1    0
##    [3,]   -1    0    1
##
## > matrixVector$tInverse(newMatrixInverse)
##

makeCacheMatrix <- function(x = matrix()) {
      matrixInversion <- NULL
      set <- function(newMatrix) {
              x <<- newMatrix
              matrixInversion <<- NULL
      }
      get <- function() x
      setInverse <- function(inversion) matrixInversion <<- inversion
      getInverse <- function() matrixInversion
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## Write a short comment describing this function
## Solves Matrix inverse for given matrix.
## Takes a matrix operation vector primed with a matrix.
## returns the solved inverse matrix. Value is calculated
## for first invocation. Value is cached and the cache is
## returned for subsequent method calls.
##
## > matrix <- matrix(c(1,3,3,1,4,3,1,3,4), nrow=3,ncol=3,byrow= TRUE)
## > matrixVector <- makeCacheMatrixVector(matrix)
## > cacheSolve(matrixVector)
##          [,1] [,2] [,3]
##    [1,]    7   -3   -3
##    [2,]   -1    1    0
##    [3,]   -1    0    1
##
## > cacheSolve(matrixVector)
##    getting cached data
##          [,1] [,2] [,3]
##    [1,]    7   -3   -3
##    [2,]   -1    1    0
##    [3,]   -1    0    1

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
