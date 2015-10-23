## These functions create a special vector to store cached values of matrices
## and their inverses and then solve for the inverse if it hasn't been cached.


## This function creates a special vector which is a list containing
## functions to set value of the vector, get the value of the vector, set
## the value of the inverse, and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the inverse of the special vector created above by
## first checking to see if the inverse has already been calculated.
## If so, is returns it from the cache.
## Otherwise, it calculates the inverse and sets the value of it in the cache
## via the setInverse function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
