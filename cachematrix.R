## The following functions solves and saves the inverse of a matrix in cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function()
      x
    setInv <- function(inv)
      m <<- inv
    getInv <- function()
      m
    list(
      set = set,
      get = get,
      setInv = setInv,
      getInv = getInv
  )
  
}


## This function computes the inverse of the special "matrix".

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if (!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
