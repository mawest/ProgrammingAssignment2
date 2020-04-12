# makeCacheMatrix and cacheSolve are functions that can be used to create a
# named matrix, for which the inverse matrix wil be cached the first time the
# calculation of the inverse is invoked. Subsequent invocations will generate
# the result from cache, thereby preventing the cost of a renewed calculation.


# makeCacheMatrix creates a named list from a matrix as argument. The list
# consists of 4 functions for setting and getting the original matrix and the
# inverse matrix (after first invocation and calculation). 
# Syntax: <VARIABLE> <- makeCacheMatrix(<MATRIX>)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(z) inv <<- z
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve gives the inverse matrix for a matrix variable that is created with
# makeCacheMatrix. If invoked for the first time the value of the invers matrix is
# calculated and cached. Subsequent invocation will get the result from cache.
# Syntax: cacheSolve(<VARIABLE>)
cacheSolve <- function(x, ...) { 
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting cached data")     
        return(inv)
    }
    message("Calculating data")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
