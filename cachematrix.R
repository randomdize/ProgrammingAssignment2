## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setMatrix <- function(solve) m <<- solve
    getMatrix <- function() m
    list(set = set, get = get,
        setMatrix = setMatrix,
        getMatrix = getMatrix)
}

## cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrix()
    ## check if the inverse has already been calculated.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if not, calculate and cache the inverse.
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrix(m)
    m
}
