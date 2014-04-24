## These functions will cache the inverse of a matrix. 

## This function creates a matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the matrix created by the makeCacheMatrix function. 
## If the inverse has already been calculated, this function will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return (i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
