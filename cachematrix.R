## These functions create and manipulate list objects that 
## bundle a matrix and its inverse.
## Non-invertible matrices are not currently supported.
## get() and getinverse() access the matrix and inverse.
## The matrix is defined with makeCacheMatrix(x) and the 
## the inverse with cacheSolve(m).  You can directly modify
## the contents with set(x) and setinverse(i), but this is
## not recommended.

## Creates a matrix-list object from a matrix and defines
## the appopriate modifier and accessor functions.

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
       getinverse = getinversemean)
}


## Returns the cached inverse of a matrix-list if it has been 
## previously computed.  Otherwise it returns the computed inverse 
## and caches the result in the matrix-list.

cacheSolve <- function(m, ...) {
  i <- m$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setinverse(i)
  i
}
