## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    get <- function() matrix
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    
    # Create the special matrix: a list with the 4 functions
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(special_matrix, ...) {
    inverse <- special_matrix$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- special_matrix$get()
    inverse <- solve(data, ...)
    special_matrix$set_inverse(inverse)
    inverse
}
