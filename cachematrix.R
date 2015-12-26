## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly .
## Coming up next, you'll find a pair of functions that cache the 
## inverse of a matrix.


## makeCacheMatrix: 
##   Creates a special "matrix" object that can cache its inverse.
##   Input: x invertible matrix
##   Output: special "matrix"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve:
##    Computes the inverse of the special "matrix" returned by 
##    makeCacheMatrix. If the inverse has already been computed 
##    and the matrix has not changed, then the function 
##    retrieves the inverse from the cache.
##    Input: x special "matrix" returned by makeCacheMatrix.
##    Output: matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
