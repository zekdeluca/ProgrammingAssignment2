# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it 
# repeatedly. The below functions cache the inverse of a matrix

# Function to create a special matrix object that can cache its inverse 
makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    set <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    get <- function() matrix
    setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)    
}

# Function to compute the inverse of a matrix returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), 
# then retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}