## cachematrix provides a way of avoiding recalculation
## of matrix's inverses, once initially calculated.


## Creates a an object (list of functions)
## to be used with cacheSolve function. 
## An invertible matrix should be input
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## sets input matrix, nullifies inverse
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    ## returns input matrix
    get <- function() x
    
    ## sets inverse, calculated externally
    setinv <- function(inv) inverse <<- inv
    
    #gets inverse (NULL if not previously set)
    getinv <- function() inverse
        
    ##returns list of functions
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)    
}


## x must be a list created by makeCacheMatrix
## cacheSolve returns its inverse
## if it was previously calculated, a cached version
## is returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    #if there is not a cached version, then calculates:
    data <- x$get()
    inverse <- solve(data, ...)
    #sets cache
    x$setinv(inverse)
    #returns calculated inverse
    inverse
}
