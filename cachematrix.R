## The following functions create a "matrix" object which stores a dynamic cache
## of its inverse. If the matrix changes it's inverse is recalculated. 


## This function constructs the matrix object, data variables, and functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL             ## used to store inverse matrix
        mirror <- x             ## used to store original matrix for comparison
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x   
        getinv <- function() inv
        setinv <- function(z){ inv <<- z}
        changed <- function(){  ## used to verify / mark matrix has changed
                change <- !identical(mirror, x) ## comparison
                mirror <<- x                    ## update mirror
                return (change)
        }
        list(set = set, changed = changed, get = get, getinv = getinv, setinv = setinv)
}

## This function calculates or returns the cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if((is.null(inverse)==FALSE) && (x$changed()==FALSE)){
                message("getting cached data")
                return(inverse)
        }
        mtrx <- x$get()
        inverse <- solve(mtrx)
        x$setinv(inverse)
        x$changed()
        inverse
}
