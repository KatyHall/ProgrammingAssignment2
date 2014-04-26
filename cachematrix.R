## Matrix inversion is usually a costly computation. 
## This pair of functions caches the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix 
## is used by cacheSolve to invert a matrix.
## This function takes a matrix as input and returns a list of functions
## The list of output variables include
## get - a function that delivers the original matrix x
## set - a function to set up at matrix
## Example
## > a$set(matrix(5:8,2))
## > a$get()
##      [,1]  [,2]
## [1,]    5    7
## [2,]    6    8
## setinverse - a function that invokes solve to invert the matrix
## getinverse - a function that returns the inverted matrix if cached and NULL otherwise

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
       x <<- y
       inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set ,get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve returns the inverse of a matrix.  
## It utilizes cached data if the matrix has already been inverted and cached.
## If the inverted matrix is not already cached, it calls solve to invert it.
## To do this, it utilizes the helper function makeCacheMatrix.
## Assumptions: Matrix is invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
       message("getting cached data")
       return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
