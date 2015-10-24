## The makeCacheMatrix function creates the matrix object with its getters and setters
## The cacheSolve function is used to get the inverse of the matrix created by makeCacheMatrix
## It either gets the cached matrix if it was previously computed or computes and caches it

## returns the special matrix object that contains getters and setters for
## both the matrix itself and its inverse
## this function behaves as the constructor of the special matrix class

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(m) {
    x <<- m
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## returns the inverse matrix of x

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
          message("getting the cached value of the inverse")
          return(inverse)
        } else {
          data <- x$get()
          inverse <- solve(data)
          x$setInverse(inverse)
          inverse
        }
}

