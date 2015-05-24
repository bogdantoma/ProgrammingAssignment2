## 'makeCacheMatrix'  creates a special "matrix", which is really a list containing a function to
##      set the value of the matrix
##      get the value of the matrix
##      set the inverse value of the matrix
##      get the inverse value of the matrix
## 'cacheSolve' calculates the inverse of makeCacheMatrix matrix


## creates a special "matrix"
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #function to set the value of the matrix
    set <- function(y) {
        ## should value be validated?
        x <<- y
        m <<- NULL
    }
    ## function to get the value of the matrix
    get <- function() x
    ## function to set the inverse value of the matrix
    setinverse <- function(inverse) m <<- inverse
    ## function to get the inverse value of the matrix
    getinverse <- function() m
    ## returned list with all function available
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## Calculates the inverse of makeCacheMatrix matrix
cacheSolve <- function(x, ...) {
    # gets the cached value of inverse
    cachedvalue <- x$getinverse()
    if(!is.null(cachedvalue)) {
        message("getting cached data")
        return(cachedvalue)
    }
    #if cached value not found calculates the inverse
    data <- x$get()
    inverse <- solve(data, ...)
    #sets the inverse value to the cache
    x$setinverse(inverse)
    inverse
}

