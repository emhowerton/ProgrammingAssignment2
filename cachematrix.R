# Title: makeCacheMatrix & cacheSolve functions
# Description: These functions allow a user to cache the inverse of a matrix instead of repeatedly computing it.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" object returned by makeCacheMatrix.  If the inverse has already been calculated, and the matrix has not changed, then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
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

# Example Use
c = rbind(c(1, -0.25), c(-0.25, 1))
c

c_cache <- makeCacheMatrix(c)
c_cache

cacheSolve(c_cache)
cacheSolve(c_cache)
