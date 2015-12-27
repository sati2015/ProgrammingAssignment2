## The functions in cachematrix.R check if an invertible matrix has an existing
## inverse matrix. If it does, then the cached result is returned. If not, the 
## inverse is computed using the R function solve(a,b,...) where a is the matrix
## whose inverse is desired and b is a default Identity matrix. a is assumed to be
## square and invertible.
## ----------------------------------------------------------

## The function makeCacheMatrix creates a special list comprising elements which 
## set, get, set the inverse and get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    matrixInv <- NULL
    set <- function(y) {
      x <<- y
      matrixInv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matrixInv <<- inverse
    getInverse <- function() matrixInv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function cacheSolve computes the inverse of the special matrix created
## by the function above. It first checks to see if the inverse already exists.
## If it exists, the cached inverse is returned else the inverse is computed using
## the R function solve. It then sets the value of the inverse in the cache via
## the setInverse function.


cacheSolve <- function(x, ...) {
    matrixInv <- x$getInverse()
    if(!is.null(matrixInv)) {
      message("getting cached data")
      return(matrixInv)
    }
    data <- x$get()
    matrixInv <- solve(data, ...)
    x$setInverse(matrixInv)
    return(matrixInv)
}
