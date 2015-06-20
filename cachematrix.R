## These functions cache the inverse of a matrix so that fewer computational resources are 
## required to calculate the inverse of the same matrix repeatedly.

## If the matrix does not have an inverse, then an error is thrown.

## The makeCacheMatrix function creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function computes the inverse of a special 'matrix' object returned by 
## makeCacheMatrix above.If the inverse has already been calculated (and hte matrix has 
## not changed), then this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
