## This function builds an object that is a matrix
## whose inverse can be cached, with the appropriate
## handlers:
## x$set(matrix) that stores matrix into x
## x$get() that reads the matrix stored in x
## x$setsolve(matrix) caches the inverse matrix
## x$getsolve() reads the stored inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(inver) m <<- inver
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve can be called on objects previously
## defined with makeCacheMatrix.
## If the inverse is not cached, it is computed, cached
## and returned. Otherwise,  cached value is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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