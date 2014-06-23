## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix Function takes a matrix as input and preserves its value.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        ## Initialize internal [x], initialize internal [m]
        x <<- y
        m <<- NULL
    }
    
    get <- function() x ## Returns [x]
    
    setSolve <- function(solve) m <<- solve ## assigns [m] contents of [mean]
    
    getSolve <- function() m ## Returns [x]
    
    list(set = set, get = get, ## Returns list of Set, get, setmean, getmean functions
         setSolve = setSolve,
         getSolve = getSolve)
    
}


## cacheSolve will search cache for the inverse of matrix - if not found it will calc. and cache the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m    
}

## Test case
# x <- matrix(c(4,3,3,2), 2, 2)
# gotit <- makeCacheMatrix(x)
# rx <- cacheSolve(gotit)
# rx
# cacheSolve(gotit)

# gotitr <- makeCacheMatrix(rx)
# cacheSolve(gotitr)
