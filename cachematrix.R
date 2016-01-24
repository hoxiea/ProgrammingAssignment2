## Put comments here that give an overall description of what your
## functions do

## This function captures a matrix and potentially caches its inverse
makeCacheMatrix <- function(x=matrix()) {
    inverse <- NULL

    reset <- function(newMatrix) {
        # Lets us change the matrix that we're encapsulating
        # Nulls out any cached inverse we might have saved, to be on the safe side
        x <<- newMatrix
        inverse <<- NULL
    }

    get <- function() {
        # Returns the matrix we've encapsulated
        return(x)
    }

    setInverse <- function(newInverse) {
        # Updates our cached inverse
        inverse <<- newInverse
    }

    getInverse <- function() {
        # Return the current cached inverse we have stowed away
        return(inverse)
    }

    return(list(reset=reset, get=get, setInverse=setInverse, getInverse=getInverse))
}



# cacheSolve is how we get the inverse of a matrix encapsulated by makeCacheMatrix
# Here, x is the result of calling makeCacheMatrix on a matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    maybeInverse <- x$getInverse()

    if (!is.null(maybeInverse)) {
        # maybeInverse wasn't null, so let's assume it was the inverse we wanted
        # So we're done! No calculations needed
        return(maybeInverse)
    } else {
        # maybeInverse was NULL, so we need to:
        # - calculate the inverse of the matrix wrapped up in x
        # - tell x to store that calculated inverse as its cached inverse value
        # - return the inverse that we just calculated
        inverse <- solve(x$get())
        x$setInverse(inverse)   # cache!
        return(inverse)
    }
}



# Using this stuff
ourmatrix <- matrix(c(1:8, 10), nrow=3)
cm <- makeCacheMatrix(ourmatrix)
cm$get()     # get back the encapsulated matrix, outmatrix
cm$getInverse()    # NULL, since we haven't done an inverse calculation yet

cacheSolve(cm)    # gets us our inverse; actually has to do the calculation
cacheSolve(cm)    # grabs the cached version
cacheSolve(cm)    # grabs the cached version
cacheSolve(cm)    # grabs the cached version
cacheSolve(cm)    # grabs the cached version
