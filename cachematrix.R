## These functions are used to calculate and cache the inverse of a matrix.
## To use, one calls makeCacheMatrix with a square matrix.  This returns
## a list of functions that can then be used by cacheSolve to cache the inverse
## of that matrix.  cache = makeCacheMatrix(m) will store m and return
## a list of functions into the variable.  Then x = cacheSolve(cache) will
## solve the inverse, store it in cacheSolve, then return it as the result.
## Future calls to cacheSolve(cache) will return the inverse directly from the
## cache.
## If the matrix is changed via makeCacheMatrix's set, it invalidates the cache
## meaning that the next call to getinv will be NULL, requiring the inverse to
## be calculated and cached again.
##
##  Code has been verified with a test suite of matrixes.


## makeCacheMatrix - This function gets a matrix and returns a list of
##  of functions that allow the expensive invert operation to be cached.
##  The functions returned are:
##    1) set - sets the value of the matrix stored
##    2) get - gets the stored matrix
##    3) setinv - sets the value of the inverse matrix
##    4) getinv - gets the stored inverse matrix
##
##  This function doesn't validate that the inverse stored is the actual
##  inverse of the stored matrix, it is a caching layer to be used by
##  another function.
##  The matrix passed in is assumed to be and needs to be square, this is
##  also not verified but assumed as per the assignment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve - This function gets a list created by makeCacheMatrix and
##  returns the inverse of the matrix stored there.  If the inverse has
##  presviously been calculated, then the operation is quicker (and displays a
##  message to show that cached value was retrieved for the result).  If not,
##  then the invserse is calcuated and stored within x, then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

