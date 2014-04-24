## The functions below create a matrix that can cache its inverse
## and return the cached inverse of the matrix if it exists, or
## compute the inverse, cache it, and then return it

## makeCacheMatrix implements a matrix that can cache its inverse
## Returns a list with four functions; set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(val) {
        x <<- val
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve computes the inverse of the matrix if it does not exist,
## caches it and returns it. Returns the cached inverse of the matrix otherwise.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse.")
        return inv
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
