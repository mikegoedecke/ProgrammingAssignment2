## 'cacheMatrix.R' contains two functions ('makeCacheMatrix' and 'cacheSolve')
## to store the value of a matrix and to calculate and cache the value of its
## inverse, so that we don't need to solve the same matrix inverse repeatedly.
## We assume for this assignment that the matrix 'x' is always invertible.
## MG 09/04/2019

## 'makeCacheMatrix' creates an object that is a list of functions to get and 
## set the value of the matrix and the value of the its inverse.

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <- inv
    getinv <- function() xinv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## 'cacheSolve' gets the value of the inverse of the matrix 'x' if it is already ## calculated and stored.
## Otherwise, it calculates and stores the value of the matrix inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if (!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    data <- x$get
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}
