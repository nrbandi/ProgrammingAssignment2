## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matrixInput = matrix()) {
##Initialize the matrix Inverse variable to NULL
## and then prepse set function to set the matrix input
    matrixInverse <- NULL
    set <- function(mInput) {
        matrixInput <<- mInput
        matrixInverse <<- NULL
    }
##prepare get function to return the input matrix
    get <- function() matrixInput
##set inverse to set the inverse matrix value in matrixInverse
## and getInerverse function will return the value
    setInverse <- function(mInverse) matrixInverse <<- mInverse
    getInverse <- function() matrixInverse
##finally make a list of all functions
    list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(mtrxInput, ...) {
## Return a matrix that is the inverse of 'x'
## First check to seee if the inverse of the matrix is already computed
## So get the value from the getInverse function  
    matrixInverse <- mtrxInput$getInverse()
## Check if the returned value is NULL or not
## If not null retrieve the data from cached variable
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
## If the value of retrieved inverse is NULL, lets computer afresh now
## First get the matrix, and then solve for the inverse matrix
    mtrx <- mtrxInput$get()
    matrixInverse <- solve(mtrx)
## Finally the setInverse function of set the value
    mtrxInput$setInverse(matrixInverse)
    matrixInverse
}
