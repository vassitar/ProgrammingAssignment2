
# The following function creates a special "matrix" object.
# This object is a list, the elements of which are four functions: "set", 
# "get", "setInverse" and "getInverse". The function "makeCacheMatrix"
# stores a matrix and caches its inverse matrix. The function "set" 
# takes a matrix as its argument - "y", and assigns it as the value of 
# the special "matrix" object "x". The function "get" returns the value 
# of the special "matrix" object. The function "setInverse" takes as its
# argument the inverse matrix of the matrix "x" and sets this value as 
# the inverse matrix ("minverse") of the special "matrix" object. 
# Finally, the function "getInverse" returns the cached inverse 
# matrix "minverse".

makeCacheMatrix <- function(x = matrix()) {
    minverse <- NULL
    set <- function(y) {
        x <<- y
        minverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse.matrix) minverse <<- inverse.matrix
    getInverse <- function() minverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


# The following function returns a matrix that is the inverse of the
# matrix "x". "x" is a special "matrix" object (a list) created with
# the function "makeCacheMatrix". The function "cacheSolve" first checks 
# if the inverse matrix has already been calculated. If this is the case, 
# it gets the inverse matrix of "x" from the cache and directly returns 
# the cached inverse matrix. If the inverse of "x" has not been calculated, 
# the function computes the inverse matrix and then sets the value of the 
# inverse matrix in the cache with the help of the function "setInverse". 
# The function "setInverse" is an element of the 
# list (special "matrix" object) "x".

cacheSolve <- function(x, ...) {
       
    minverse <- x$getInverse()
    if(!is.null(minverse)) {
        message("getting cached data")
        return(minverse)
    }
    m <- x$get()
    minverse <- solve(m, ...)
    x$setInverse(minverse)
    return(minverse)
}
