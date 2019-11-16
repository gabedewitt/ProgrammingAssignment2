## This first function will create a list set of functions that will be passed
## to cacheSolve and if the calculation of the inverse was already done
## it will be returned.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The way that this function works is by passing the result of makeCacheMatrix
## as an argument, by using it it's possible to determine if a calculation was
## done previously to the same matrix, therefore it gets the cached inversed 
## matrix and returns it without performing a new calculation.

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
        ## Return a matrix that is the inverse of 'x'
}
