## These functions create a special matrix object that can cache its inverse.
## If the inverse has already been calculated, it will return the cached value
## instead of recomputing it, which improves computational efficiency.
## This function creates a special "matrix" object that can store a matrix
## and cache its inverse once it has been calculated.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special matrix created by
## makeCacheMatrix. If the inverse has already been computed and cached,
## it retrieves the cached value. Otherwise, it calculates the inverse,
## stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {
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
