## Function for Caching the Inverse of the matrix
## When calculating an inverse of a matrix, some time it is more efficient
## to store the inverse that already calculated in a cache for a later use
##below are functions for create an object that stores a matrix and cache its inverse

## This function creates a special matrix to store its inverse in a cache

makeCacheMatrix <- function(x = matrix()) {
 	invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invmat <<- inverse
        getinverse <- function() invmat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of a matrix that created by the 
##function above (makeCacheMatrix). If the inverse has already been calculated
##then the value will be pulled from the cache

cacheSolve <- function(x, ...) {
       
        invmat <- x$getinverse()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        matrix <- x$get()
        invmat <- solve(matrix, ...)
        x$setinverse(invmat)
        invmat
 ## Return a matrix that is the inverse of 'x'
}
