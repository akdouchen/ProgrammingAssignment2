## Put comments here that give an overall description of what your
## functions do
# calculate and cache the inverse of matrix (CPU Cycle intensive). When the inverse of matrix is needed again, it can be looked up in the cache rather than recomputed.


## Write a short comment describing this function
# The function, makeCacheMatrix creates a special "vector", which contains a list of functions to:
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.setInverse the inverse of matrix
# 4.getInverse the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve function calculates the inverse of matrix x created with the function makeCacheMatrix. 
# It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}
