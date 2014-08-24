## These two functionms let you cache the inverse of a matrix. In order to do
## so, you create a matrix object with the makeCacheMatrix function. With
## that object, you can use cacheSolve to compute the inverse of the matrix. If
## that matrix's inverse has already been computed, the cached version will be 
## returned.

## makeCacheMatrix lets you create a matrix object which can be used to 
## cache the inverse of m. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(newinv) inv <<- newinv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve either computes the inverse of your matrix object or returns the 
## cached inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
