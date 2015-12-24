## Overall comments:
## ## This pair of functions (makeCacheMatrix, cacheSolve) are designed to cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse matrix as NA
  inv <- matrix()
  ## set function to assign matrix value
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  ## get function to pull matrix value
  get <- function() x
  ## set inverse matrix
  setinv <- function(inverse) inv <<- inverse
  ## get inverse matrix
  getinv <- function() inv
  ## creast a list object
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## First check if the inverse has been calculated
  inv <- x$getinv()
  if(!all(is.na(inv))) {
    message("getting cached data")
    return(inv)
  }
  ## Otherwise, calculate the inverse and cache it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
