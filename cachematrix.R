
## makeCacheMatrix returns a list of functions 
## that performs operations on a mtrx variable in a global environment: 
## set and get the value of 'mtrx' and its inverse

makeCacheMatrix <- function(mtrx = matrix()) {
     
    mtrx_inv <- NULL
    set <- function(mtrx_new) {
            mtrx <<- mtrx_new
            mtrx_inv <<- NULL  
    }
    get <- function() mtrx
    setinv <- function(mtrx_inv_new) mtrx_inv <<- mtrx_inv_new
    getinv <- function() mtrx_inv
    list(set = set, get = get,setinv = setinv, getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix or
## returns the cashed matrix from the global environment

cacheSolve <- function(mtrx, ...) {
    
        ## Recalculate the inversed matrix 
        ## if it has not been calculated yet (set or reset to NULL), and
        ## if the original matrix matrix has been set to smth other than NULL
    if ( is.null(mtrx$getinv()) && !is.null(mtrx$get()) ) mtrx$setinv( solve(mtrx$get(), ...) ) 
        
        ## return the inversed matrix from the global environment
    mtrx$getinv()                                   

}
