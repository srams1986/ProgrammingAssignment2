## Put comments here that give an overall description of what your
## functions do

## This function is use to set and get the values of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(xinv) inv<<- xinv
  getinv <- function() inv
  
  list(set = set, get = get, setinv=setinv, getinv=getinv)
  
}


## Solves for the inverse of the matrix, If value already available, returns from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  matinv <- x$getinv()
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  
  data <- x$get()
  matinv <- solve(data, ...)
  x$setinv(matinv)
  matinv
  
}
