## The function makeCacheMatrix will create a matrix objects and cache it

## The function cacheSolve evalautes the inverse of the matrix obtained from makecacheMatrix function

## makeCacheMatrix is a function that creates special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  specMatrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    specMatrix <<- NULL
  }
  
  
  getMatrix <- function() x
  
  setInverse <- function(invMat) 
    
    specMatrix <<- invMat
  
  getInverse <- function() specMatrix
  
  
  list(
    setMatrix = setMatrix, 
    getMatrix = getMatrix,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## The function cacheSolve should return the inverse of the matrix created by makeCacheMatrix function.
## cacheSolve retrieves the inverse whenever the cached inverse is available
## if the inverse is not cached the cacheSolve computes the inverse of the special matrix amnd returns this inverse after caching it

cacheSolve <- function(x, ...) {
  
  
  specMatrix <- x$getInverse()
  
  if(!is.null(specMatrix)) {
    
    return(specMatrix)
    
  }
  
  dat <- x$getMatrix()
  
  specMatrix <- solve(dat, ...)
  x$setInverse(specMatrix)
  return(specMatrix)
}