## Two functions allowing for the inverse of a matrix to be cached
## after being computed once.

## makeCacheMatrix function creates a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Initially no inverse has been computed:
    inv <- NULL
    # Allow to set new values for a matrix. Any previously
    # computed inverse will no longer be valid:
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    # Allow to retrieve the current matrix:
    get <- function() x
    # Allow to set the inverse of the matrix:
    setinverse <- function(newinv) inv <<- newinv
    # Allow to retrieve the current inverse:
    getinverse <- function() inv
    # Return the special cache "matrix" (i.e. a list of values
    # to store)
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function sets and returns the inverse of the special
## "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {   
    # Get the current inverse value stored in 'x':
    inv <- x$getinverse()
    # If current value is not NULL the inverse has already been
    # computed and set in 'x'; Return it:
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # If current value is NULL the inverse has to be computed.
    # First get the matrix:
    data <- x$get()
    # Then compute the inverse:
    inv <- solve(data, ...)
    # Finally set the inverse value in 'x' and return that inverse
    x$setinverse(inv)
    inv
}
