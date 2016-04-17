## the functions below are for creating the inverse of a matrix that i shall create 
## and for giving messages whenever an action(in this case the inverse of the matrix) has been repeated.

## below i create a special matrix object that can cache its own inverse including confirmation whether an inverse had been done before.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
 set <- function(y) {
 x <<- y
 inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
get = get,
setInverse = setInverse,
getInverse = getInverse)
}


## the function below gets the inverse of the matrix created in the function above. It checks whether the inverse of the matrix has already
##been done and if it has it should return the inverse
##otherwise it should return the inverse of the matrix
cacheSolve <- function(x, ...) {
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                return(inv)
        }
	  else mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
}
