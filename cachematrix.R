## Functions that allow one to create a matrix 'container'
## for caching the inverse of a matrix

## Function to create a special matrix 'object' that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m

    # create a list of all the function calls
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function to compute the inverse of the special 'matrix' object 
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) 
    x$setinverse(m)
    m
}
