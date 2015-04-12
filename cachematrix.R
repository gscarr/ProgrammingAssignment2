## Functions that cache a matrix and lazily compute its inverse.  The inverse 
## is also cached so the computation can be avoided if the inverse is requested
## again.  To use these functions, first call makeCacheMatrix.  If you pass in 
## a matrix m you can immediately pass the returned object (cm) to cacheSolve 
## to compute and return the inverse.  Alternatively, if you don't provide m at
## the start, you call cm.setmat(m) before you pass cm to cacheSolve. If you 
## need to simultaneously cache the inverse of more than one matrix you can
## call makeCacheMatrix separately for each of them.

## Creates a cachedMatrix object that can subsequently be passed to
## cacheSolve to compute and cache the inverse matrix. You can also call
## cachedMatrix.setmat(m) to cache a new matrix to be inverted.
makeCacheMatrix <- function(mat_ = matrix()) {
    inv_ <- NULL # slot for inverse
    
    ## create accessor functions for the mat_ and inv_ slots
    ## Note: these are local variables of makeCacheMatrix which are captured in
    ## the closure environment for these functions so they can be accessed by
    ## the functions without polluting the global namespace.
    
    setmat <- function(mat) {
        mat_ <<- mat
        ## NULL any previous inverse to force a new computation the
        ## next time cacheSolve is called.
        inv_ <<- NULL 
    }
    getmat <- function() mat_
    setinv <- function(inv) inv_ <<- inv
    getinv <- function() inv_
    
    ## Return a list of the accessor functions that will
    ## set/get a matrix in the variable 'mat_' and its inverse in 'inv_'. 
    list(setmat = setmat, getmat = getmat,
         setinv = setinv,
         getinv = getinv)
}


## Uses solve to compute the inverse of cachedMatrix and caches the result
cacheSolve <- function(cachedMatrix, ...) {
    inv = cachedMatrix$getinv()
    if(!is.null(inv)){
        message("returning cached data")
        return(inv)
    }
    ## compute inv of cached mat
    mat = cachedMatrix$getmat()
    inv <- solve(mat)
    cachedMatrix$setinv(inv)
    inv
}
