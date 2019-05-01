## These functions store a matrix inverse in the cache
## for Programming Assignment 2

## This function creates a special "matrix" object that can cache its inverse.

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


## WThis function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- lapply(x, solve)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- lapply(x, get)
  m <- solve(data, ...)
#  lapply(m, setinverse)
  x$getinverse(m)
  m
}
