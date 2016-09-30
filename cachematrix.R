## Programming Assignment 2
## These functions allow to take a matrix object and cache the values of the inverse of the matrix without
## calling the initial matrix object. 
##

## makeCacheMatrix() takes a matrix, creates a list object with methods applied to the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cachesolve takes the list object created in makeCacheMatrix(), then returns the inverse of the matrix
## found via the get() method in makeCacheMatrix() if there's no cached value already in the list object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
