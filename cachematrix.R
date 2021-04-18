## Caching the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix"
## object that can cache its inverse, which contains 4 functions,
## 1. set the matrix            2. get the matrix
## 3. set the inverse matrix    4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse=matrix()) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The CacheSolve function calculates the inverse matrix of
## the special matrix created by makeCacheMatrix. If the inverse
## has been calculated, it gets the inverse matrix direct from
## the cashe, otherwise, it sets the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
