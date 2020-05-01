## Assignment: Caching the Inverse of a Matrix
## Caching the inverse of a matrix if the matrix is not changed.


# First function takes regular matrix as an argument
# and creates a special object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #set inverted matrix to NULL upon initiation
  set <- function(y) { # function for changing initial matrix
    x <<- y     #replace value of x with new value of y
    m <<- NULL  #set inverted matrix back to NULL
  }
  get <- function() x
  setinverse <- function(solved) m <<- solved #set the inverted matrix to the value from cacheSolve function
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The following function calculates inverse of the special "matrix"
# If the inverse has already been calculated (and the matrix has not changed), then
# `cacheSolve` retrieves the inverse from the cache.

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
