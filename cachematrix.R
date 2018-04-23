makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  seti <- function(inverse) inv <<- inverse
  geti <- function() inv
  list(set=set, get=get, seti=seti, geti=geti)
}
cacheSolve <- function(x, ...) {
  inv <- x$geti()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$seti(inv)
  inv
}

