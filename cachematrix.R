
## makeCacheMatrix returns a list of functions 
## that performs operations on a mtrx variable in a global environment: 
## set and get the value of 'mtrx' and its inverse

makeCacheMatrix <- function(mtrx = matrix()) {
    mtrx_inv <- NULL
    setmtrx <- function(mtrx_new) {
        if (!is.null(mtrx_new) && is.matrix(mtrx_new) && all.equal(mtrx,mtrx_new)){
            mtrx <<- mtrx_new
            mtrx_inv <<- NULL  
        }
    }
    getmtrx <- function() mtrx
    setinv <- function(mtrx_inv_new) mtrx_inv <<- mtrx_inv_new
    getinv <- function() mtrx_inv
    list(set = set, get = get,setinv = setinv, getinv = getinv)
        
}

## cacheSolve calculates the inverse of the matrix or
## returns the inversed matrix that was cashed in the global environment

cacheSolve <- function(mtrx_eval, ...) {
    if(!isnull(mtrx_eval) && is.matrix(mtrx_eval)){ # check if new matrix is not NULL and is a matrix
        
        # get the matrix and inversed matrix from the global environment
        mtrx_stored <- mtrx$getmtrx()               
        mtrx_inv_stored <- mtrx$getinv()
        
        #Recalculate the inversed matrix if:
        #- the matrix that is stored in the global environment is NULL, or 
        #- the inverse of the matrix has not been calculated yet (is NULL), or
        #- new matrix is different from the one that is stored in the global environment
        if (is.null(mtrx_stored) || is.null(mtrx_inv_stored) || !all.equal(mtrx_stored,mtrx_eval)){ 
            mtrx$setmtrx(mtrx_eval)                 # save the new matrix to the global environment    
            mtrx$setinv(solve(mtrx_eval, ...))      # calculate and save the new inversed matrix to the global environment
        }
    }
    mtrx$getinv()                                   # return the inversed matrix from the global environment
}
