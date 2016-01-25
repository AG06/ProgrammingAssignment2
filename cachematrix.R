## These functions together are used to compute the inverse of a matrix x. However, if the inverse has been previously 
## computed, then it is cached in memory and is available for retrieval.

## Function to create a matrix that can cache its inverse and has functions that get/set the original matrix and 
## get/set the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## Function computes the inverse of matrix "x" unless it is already pre-computed. If latter is true, then
## it simply calls the cached value of the inverse of x.

cacheSolve <- function(x, ...) {
       
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
