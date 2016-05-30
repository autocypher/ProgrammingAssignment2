## A set of functions to get the inverse of a matrix, using
## a specialised contruction function, which takes a matrix
## as an argument, and a function to query and interact with
## the caching class.

# The function takes a standard R matrix and creates and
# returns a list containing functions to use for caching
# the output of the solve (inverse) matrix function.
makeCacheMatrix <- function(x = matrix()) {
    cinv <- NULL
    set <- function( y ) {
        x <<- y
        cinv <<- NULL
    }
    get <- function() {
        invisible(x)
    }
    setinv <- function( inv ) {
        cinv <<- inv
    }
    getinv <- function() {
        invisible(cinv)
    }
    invisible( list( set = set, get = get,
          setinverse = setinv, getinverse = getinv ) )
}

# cacheSolve returns a matrix that is the inverse of
# the special matrix 'x' (created by makeCacheMatrix),
# by making use of the caching capabilities if the 
# makeCacheMatrix function.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if( ! is.null( m ) ) {
        message( "getting cached data" )
        return ( m )
    }
    data <- x$get()
    m <- solve( data, ... )
    x$setinverse( m )
    m
}
