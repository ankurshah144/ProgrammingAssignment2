## This function creates a special "matrix" object that can cache its inverse
## Created for Coursera - R Programming Assignment 2

makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL  # Initialize matrix inverse to NULL
    set <- function(y) {
        x <<- y            # The matrix that will be used to calculate inverse
        mInverse <<- NULL  # Initialize inverse to NULL for every new matrix
    }
    get <- function() { x }  # Return matrix
    setInverse <- function(inverseMatrix) {
        mInverse <<- inverseMatrix  # Calculate inverse using 'solve' function in R
    }
    getInverse <- function() { mInverse }  # Return inverse of original matrix

    list(setM = set, getM = get,
         setInvM = setInverse,
         getInvM = getInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    mInverse <- x$getInvM()  # Fetch current value of object containing inverse
    if(!is.null(mInverse)) {  # Check if the object is not null
        message("getting cached data")
        return(mInverse)  # Return cached inverse
    }
    data <- x$getM()
    mInverse <- solve(data, ...)
    x$setInvM(mInverse)
    mInverse
}