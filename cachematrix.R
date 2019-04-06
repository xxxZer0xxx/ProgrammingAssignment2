## creating to functions, makeCacheMatrix and cacheSolve
## to calculate the inverse a given square matrix
## aswell as cache it to save future resources.

## makeCacheMatrix sets up the matrix object
## so that it is callable for the cacheSolve
## function. If a new object is created the 
## cache will be cleared.

makeCacheMatrix <- function(x = matrix()) {
  
  invX <- NULL
  set <- function(y){
    x <<- y
    invX <<- NULL
  }
  get <- function()x
  setinverse <- function(solve) invX <<- solve 
  getinverse <- function() invX
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will calculate the inverse of 
## the matrix given in makeCacheMatrix
## and will return the inversed Matrix.


cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  invX <- x$getinverse()
  if(!is.null(invX)){
    message("getting cached inversed matrix")
    return(invX)
  }
  m <- x$get()
  invX <- solve(m)
  x$setmean(invX)
  invX
}
