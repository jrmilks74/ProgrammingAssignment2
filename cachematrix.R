## These functions solve for the inverse of any square matrix and then cache the answer.

## makeCacheMatrix creates a matrix, 1) sets the values included in the matrix, 2) gets the values, 3) sets the value of the inverse of the matrix, 4) gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Takes the matrix output of the makeCacheMatrix function and returns its inverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
