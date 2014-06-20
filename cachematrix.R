## Based on the example from Programming Assignement #2
## Contains two functions:
##   - makeCacheMatrix : will create a "cache-friendly" matrix
##   - cacheSolve      : will "solve" the matrix and cache the result

## Creates a cache-friendly matrix
## Example: m = makeCacheMatrix(matrix(c(4, 2, 7, 6), 2, 2))

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This will solve the matrix and cache the result
## Example: m = makeCacheMatrix(matrix(c(4, 2, 7, 6), 2, 2))
##          cacheSolve(m)
##          cacheSolve(m) # should produce the cached message

cacheSolve <- function(x, ...) {
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
