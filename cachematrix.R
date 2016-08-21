## Inverting a matrix is costly.  The following functions cache the computations, rather than calculating it repeatedly.

## Here we have a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  
  set <-function(y) {
    x <<- y
    inversed <<- NULL
  }
  
  get <- function() x
  setInversed <- function(newInversed) inversed <<- newInversed
  getInversed <- function() inversed
  list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}


## The function below calculates the inverse of the matrix
## returned by makeCacheMatrix above. If the inverse has already 
## we computed in the function above.  If we already have an inverse
## then cachesolve will pull the inverse that has already been computed. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversed <- x$getInversed()
  
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  
  data <- x$get()
  inversed <- solve(data, ...)
  x$setInversed(inversed)
  
  inversed
}