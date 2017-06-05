##

## The function makeCacheMatrix creates special "matrix" object 
##that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes  inverse matrix and if it is already exists, then it 
## retrieve it from cache

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
