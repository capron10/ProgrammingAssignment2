## The purpose of these 2 functions is to cache potentially time-consuming computation of the inverse of a matrix.


## makeCacheMatrix creates a special "matrix" as a list of functions to set/get the value of the matrix and set/get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix.  
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv() function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    ## If inverse is already cached, return the cached value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## Otherwise, calculate, set, and get the inverse using solve()
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
