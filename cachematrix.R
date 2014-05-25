## This is a programming assignment as part of the R coourse which involves creating 2 functions
## These functions will help save computations in a cache, so they can be accessed multiple times
## without evaluating every time, thereby saving system memory


## The first function, makeVector creates a special "matrix", which is really a list containing a function to
## set the value of the matrix (if not already in cache)
## get the value of the matrix (if already in cache)
## set the inverse of the matrix (if not already in cache)
## get the inverse of the matrix (if already in cache)


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


## The following function calculates the inverse of the matrix if it is not already existing in the cache

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
