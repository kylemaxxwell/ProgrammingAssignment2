## Matrix inversion is usually a costly computation. The function
## wiil cache potentially time-consuming computations.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    #set the value of matrix
    set <- function(newMatrix) {
        x <<- newMatrix
        inverseMatrix <<- NULL
    }
    #get the value of matrix
    get <- function() x
    #set the value of the solve
    setsolve <- function(solve) inverseMatrix <<- solve
    #get the value of the solve
    getsolve <- function() inverseMatrix
    #list the sub function
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the same matrix), then the cachessolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getsolve()
    #return makeCacheMatrix got the matrix and jumb out of the function
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    #get matrix from makeCacheMatrix and inverse it then set the inverse
    #result to makeCacheMatrix
    matrixData <- x$get()
    inverseMatrix <- solve(matrixData, ...)
    x$setsolve(inverseMatrix)
    inverseMatrix
}
