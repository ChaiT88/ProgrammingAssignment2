## Note 1: makeCacheMatrix function creates a matrix that saves a matrix and its
## inverse in a separate cache
## Note 2: cacheSolve.R function computes the inverse of a matrix and saves both
## the matrix and inverse in the cache. If the inverse has already been 
## calculated, that value is extracted from the cache and displayed
## Note 3: The purpose of creating this kind of cache is to make programs 
## running multiple computations of large datasets work faster.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Now that a cache for all the values has been established, the computational
## part of the code can follow

cacheSolve <- function(x, ...) {
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
## The returned value is the inverse of the matrix that the user inputs