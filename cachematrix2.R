## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. 

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL ## defines s as NULL
    set <- function(y) { ## defines the function "set" that defines x as a user-defined matrix y
        x <<- y
        s <<- NULL
    }
    get <- function() x ## defines the function "get" that prints the cached x
    setsolve <- function(solve) s <<- solve ## defines the function "setsolve" that executes the function solve to obtain the inverse of x
    getsolve <- function() s ## defines the function "getsolve" that prints the cached inverse of x
    list(set = set, get = get, ## returns a list containing 4 functions
         setsolve = setsolve,
         getsolve = getsolve)
}

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}



## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x if the inverse has not been cached

    s <- x$getsolve() ## set s as the cached matrix that is the inverse of x
    if(!is.null(s)) { ## if condition that prints pre-determined message if a cached matrix is present
        message("getting cached data")
        return(s)
    }
    data <- x$get() ## if a cached matrix is absent, the x$get() function proceeds to execute function solve to obtain the inverse of x
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}