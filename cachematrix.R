## Code from Caching the Mean of a Vector was used as
## the base code for this assignment.

## This function creates a matrix whose inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
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

## This function takes the matrix from the above function and
## caluclates its inverse. If that inverse for that matrix
## has already been used, it returns a message.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
