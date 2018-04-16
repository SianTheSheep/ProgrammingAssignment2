## Create a set of functions that will calculate
## the inverse of a matrix and cache the result

## Create the matrix and supporting functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list (set = set, get = get, 
              setinverse = setinverse, getinverse = getinverse)
        
}


## Compute the inverse of the matrix, if already calculated return
## the cached value

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


