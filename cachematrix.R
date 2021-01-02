
## The functions in this script allow for the creation of objects that
## store a matrix together with its inverse. Retrieval of the inverse matrix
## is done from cache when available, otherwise the inverse is calculated
## and stored in cache.
## Using these functions makes sure that the costly operation of calculating
## the inverse of a matrix is done only once. Every following request for the
## inverted matrix is served form the cache (but only when the original matrix
## has not changed).

## The function 'makeCacheMatrix' creates a list with four pointers to other
## functions, namely 'set' and 'get' for the original matrix and
## 'setInv' and 'getInv' for the inverse of the matrix.
## Because these pointers to the created functions are preserved in the
## returned list, the whole environment of the function 'makeCacheMatrix'
## is preserved after execution. Therefore, the variables 'x' and 'inv' also
## remain accessible in memory.
makeCacheMatrix <- function(x = matrix()) {
    # The variable x is always defined since it is a formal argument with
    # a default value.
    # The variable "inv" is initialised with the NULL value
    inv <- NULL
    # With the function 'set', a new matrix can be assigned to the object.
    # The value for 'inv' is set to NULL again since the cached inverse would
    # no longer correspond to the new matrix.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # With the function 'get', the original matrix 'x' is returned
    get <- function() x
    # The function 'setInv', stores the inverse of the original matrix x
    # in the variable 'inv'
    setInv <- function(invMatrix) inv <<- invMatrix
    # With the function 'getInv', the inverse of the matrix is returned.
    # The value could be NULL, therefore, the function 'cacheSolve' must be
    # used to obtain the inverse of the matrix.
    getInv <- function() inv
    # A list with pointers to the above functions is created and returned
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv
    )
}


## The function 'cacheSolve' returns the inverse of the matrix stored in an
## object that was created by the function 'makeCacheMatrix'.
## If the inverse is available, it is served from the cache. If not, then the
## inverse is calculated and stored for future requests.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    ## checking whether the inverse is already stored in cache.
    if(!is.null(inv)) {
        ## the inverse is served from cache and this function ends with the 
        ## 'return'-statement
        message("Fetching cached inverted matrix.")
        return(inv)
    }
    ## When the inverse is not available yet, then it is calculated from the 
    ## original matrix from the object created by 'makeCacheMatrix'
    matrix <- x$get()
    inv <- solve(matrix, ...)
    # the inverse matrix is stored in cache
    x$setInv(inv)
    # the inverse is returned
    inv
}

