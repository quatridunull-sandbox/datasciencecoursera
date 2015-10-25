## These functions create a matrix with a cached inverse value and cache that value when requested

## This function instantiates a cached matrix.

makeCacheMatrix <- function(values = matrix()) {
  inverse <- NULL
  set <- function(inputMatrix) {
    values <<- inputMatrix
    inverse <<- NULL
  }
  get <- function() values
  setinverse <- function(inverse_in) inverse <<- inverse_in
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## For an initialized CacheMatrix x, cachcSolve will output the inverse if it is
## already initialized. Otherwise, it will calculate the inverse, set the cached
## inverse value of x to its calculated value, and output the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
