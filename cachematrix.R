## These two functions that are used to create a special object
## that stores a matrix and caches its inverse to avoid the 
## costly computation associated with this.
##
## Phillip Owens 01 December 2017

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {x}
    setinverse <- function(solve) {i <<- solve}
    getinverse <- function() {i}
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Check if inverse is cached
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting inverse from cache")
        return(i)
    }
    ## Matrix not cached - compute and return
    data <- x$get()
    i <- solve(data)
    ## Return a matrix that is the inverse of 'x'
    x$setinverse(i)
    i
}
