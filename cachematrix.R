## These functions cache the inverse of a matrix.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {    ## Function that sets the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() {     ## Function that gets the matrix
                x
        }
        setinvmatrix <- function(solve) {      ## Function that sets the inverse 
                m <<- solve
        }
        getinvmatrix <- function() {     ## Function that gets the inverse
                m
        }
        list(set = set, get = get,       ## Returns a list of the functions
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinvmatrix() ## Returns a matrix which is inverse of x
        
        if(!is.null(m)) {       ## Returns the inverse if it is set
                message("getting cached data")
                return(m)
        }
        matrixdata <- x$get     ## Gets the matrix object
        m <- solve(matrixdata, ...)     ## Calculates the inverse
        x$setinvmatrix(m)       ## Sets the inverse to the matrix object
        m      ## Returns a matrix that is the inverse of 'x'
}
