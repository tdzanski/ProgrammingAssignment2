## makeCacheMatrix loads a supplied matrix into memory
## cacheSolve returns the inverse of the supplied matrix

## makeCacheMatrix() creates an object that stores a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve returns the inverse of x using the cached matrix in the makeCacheMatrix's enviornment
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    ## Return a matrix that is the inverse of 'x'
    m
}

#makeCacheMatrix and cacheSolve tests
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
mcm1 <- makeCacheMatrix(m1)
mcm2 <- makeCacheMatrix(m2)
m1i <- cacheSolve(mcm1)
m2i <- cacheSolve(mcm2)
id1 <- m1 %*% m1i
id2 <- m2 %*% m2i


