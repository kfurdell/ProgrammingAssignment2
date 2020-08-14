## This set of functions caches the inverse of a matrix and retrieves that cached value.

## makeCacheMatrix creates a matrix and caches the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
           x <<- y
           m <<- NULL
     }
     
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     
     list (set = set, get = get, setinverse = setinverse, 
           getinverse = getinverse)
}



## cacheSolve returns the inverse of a matrix. 
## It retrieves the inverse from the cache if it exists, 
## if not it calculates the inverse and sets its value in the cache.

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
       message("getting cached data")
       return(m)
     }

     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}