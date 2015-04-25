## The makeCacheMatrix associates 4 functions (set, get, getInverse,
## setInverse) with a matrix. The cacheSolve function uses these functions
## to retrieve the inverse matrix if available and computes the matrix if 
## not available

## makeCacheMatrix. Accepts a matrix as input, and associates 4 functions with
## it. These 4 functions allow caching the matrix & inverse of matrix instead
## of computing again and again

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(n) {
    x <<- n
    xInv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(nInv) xInv <<- nInv  
  getInverse <- function() xInv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## caceSolve. This function tries to fetch the cached inverse of a matrix.
## When not available, will compute the inverse, caches and returns it.

cacheSolve <- function(x, ...) {
  xInv <- x$getInverse()
  if (!is.null(xInv)) {
    message("getting cached matrix inverse")
    return(xInv)
  }
  xInv <- solve(x$get(), ...)
  x$setInverse(xInv)
  xInv
}