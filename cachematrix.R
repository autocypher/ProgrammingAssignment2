## Put comments here that give an overall description of what your
## functions do

# The function takes a standard R matrix and creates a
# list containing functions to perform on the matrix.
# These include the usual get/set and alse the
# {get/set}inverse which are used to cache and retrieve
# the cached inverse of the marix
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
# the speical matric 'x' (created by makeCacheMatrix)
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if( ! is.null( m ) ) {
        message( "getting cached data" )
        return ( m )
    }
    data <- x$get()
    m <- solve( data )
    x$setinverse( m )
    m
}
