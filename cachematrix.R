## These functions introduce special matrix object and its inverse

## This function creates a cached matrix object.
## provides a list of functions to set & get the matrix object and its inverse

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  
  get <- function() x
  getInv <- function() invX
  setInv <- function(invA) invX <<- invA 
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function accepts a list and returns the inverse of a matrix from cache or by calling solve method.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invX <- x$getInv()
  if(!is.null(invX)) {
    message("Getting from cache")
    return (invX)
  }
  a <- x$get()
  invX <- solve(a)
  x$setInv(invX)
  invX
}