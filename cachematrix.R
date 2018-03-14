## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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