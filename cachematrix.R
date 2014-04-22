## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## set the value of the inversion to NULL
    xinversion <- NULL 
    set <- function(y) {
        x <<- y
        xinversion <<- NULL 
    }
    
    get <- function() x ## get the value of matrix
    setInverse <- function(inversion) xinversion <<- inversion ## set of the inversion matrix
    getInverse <- function() xinversion ## get the value of inversion matrix
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse() # get the inversed matrix
    if(!is.null(m)) { 
        message("getting cached data")
        return(m) # return the calculated inversion
    }
    data <- x$get() # if m is NULL: do x$get to get the matrix object
    m <- solve(data) # solve matrix object
    x$setInverse(m) # set solved object to m
    m # return the solved result
}
