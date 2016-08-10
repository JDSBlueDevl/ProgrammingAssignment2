## The overall focus of these functions is to store a matrix call
## and its inverse in cache.

## The function makeCacheMatrix() creates the environment which
## stores the matrix in cache and defines 'setters' and 'getters'
## with which to store the results in cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve() function does three things: determines if the
## matrix stored in makeCacheMatrix() is square and invertible, 
## calculates the inverse of an invertible matrix, and recalls
## the calculation from cache if it has been previously
## calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  if(ncol(x$get()) != nrow(x$get())) {
    message("not a square matrix")
    x$setinv("not a square matrix")
  } else {
    det <- det(x$get())
    if(det == 0) {
      message("matrix not invertible")
      x$setinv("matrix not invertible")
    } else {
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
    }
  }
}