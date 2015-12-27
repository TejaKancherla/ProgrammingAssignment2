## makeCacheMatrix(): creates a special "matrix" object that can cache its inverse.
## cacheSolve(): computes the inverse of the "matrix" returned by 
## makeCacheMatrix(). If the inverse has already been calculated and the
## matrix has not changed, it'll retrieves the inverse from the cache directly.

## First function makeCacheMatrix creates a special "matrix", which is a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)

}


## The following function calculates the inverse of the matrix  
## created with the above function. However, it first checks to see if the
## inverse of the matrix has already been calculated. If so, it gets the inverse matrix
## from the cache and skips the computation. Otherwise, it calculates the Inverse of the matrix
## and sets the value of the matrix in the cache via the setmmatrix function.

cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
     
}

