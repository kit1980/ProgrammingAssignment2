## makeCacheMatrix and cacheSolve can be used
## to cache time-comsuming matrix inversion operations.

## makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
##     - set the value of the matrix
##     - get the value of the matrix
##     - set the value of the inversion
##     - get the value of the inversion
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinversion <- function(inversion) inv <<- inversion
    getinversion <- function() inv
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
}

## cacheSolve calculates the inversion of the special "matrix"
## created with the makeCacheMatrix function.
## However, it first checks to see if the inversion has already been calculated.
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the inversion of the data and sets the value of
## the inversion in the cache via the setinversion function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinversion()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinversion(inv)
    inv
}
