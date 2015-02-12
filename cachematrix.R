## Pair of functions to cache a matrix and compute its inverse

## makeCacheMatrix :: given a matrix x, returns a
## list that contains a set of functions to compute 
## the inverse of x using a cached copy of x

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## cacheSolve :: given a list created by makeCacheMatrix
## which contains a reference to the cached matrix and
## its inverse (if previously computed) returns the inverse
cacheSolve <- function(x, ...) {
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