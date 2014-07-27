## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix - creates a cache matrix
## cacheSolve - returns inverse of matrix either computed when new data / cached when old data

## Write a short comment describing this function
## makeCacheMatrix - creates a new cache matrix - defines set, get, setSolve, getSolve functions
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(Solve) m <<- Solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Write a short comment describing this function
## cacheSolve - returns the inverse of the matrix x. 
## Computes inverse if not already solved. Once solved, it caches the result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
