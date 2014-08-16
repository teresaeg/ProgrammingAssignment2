## R Programming Coursera Assignment 2

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()  ## try to get the inverse
  
  ## if the inverse already exists, great!
  if(!is.null(inverse)) {
    message("getting cached data")  ## print the message showing that it already existed
    return(inverse)  ## return the already existing inverse
  }
  
  ## if the inverse did not exist yet...
  data <- x$get()  ## get the matrix from its other location
  inverse <- solve(data, ...)  ## find the inverse of the matrix
  x$setinverse(inverse)  ## save its inverse for future reference
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}
