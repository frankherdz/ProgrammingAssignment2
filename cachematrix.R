## The following functions creates and returns a special matrix object that can cache its inverse.

## makeCacheMatrix
## Parameters: x datatype matrix
## Return: makeCacheMatrix special matrix object
## This function creates a special "matrix" object that can cache its inverse.
## Special matrix list of functions:
##  get() -> Get matrix value
##  set() -> Set matrix value
##  setSolve() -> Set inverse matrix value
##  getSolve() -> Get inverse matrix value


makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL  
    set <- function(y) { ## Define set matrix function
        x <<- y
        m <<- NULL
    }
    get <- function() x  ## Define get matrix function
    setSolve <- function(solve) m <<- solve     ## Define set inverse matrix function
    getSolve <- function() m    ##  getSolve -> Define get inverse matrix function
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve) ## Creates list of functions

}


## cacheSolve
## Parameters: x datatype makeCacheMatrix
## Return: Inverse of x matrix. . If the inverse has already 
##         been calculated then it is retrieve from the cache.
## This function returns the inverse matrix and if it has already been calculated 
## then it is retrieve from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()  ## Get inverse matrix value
    if(!is.null(m)) { ## Check if value was retrieved before.
        message("getting cached data") ## Message to display that the value was retrieve from cache
        return(m) ## Return cache value of inverse matrix.
    }
    data <- x$get() ## Get matrix value
    m <- solve(data, ...)  ## Calculate inverse matrix
    x$setSolve(m) ## Set inverse matrix value
    m  ## Return inverse matrix value
}
