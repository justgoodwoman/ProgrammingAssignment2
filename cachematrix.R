makeCacheMatrix <- function(x = matrix()) {
  y = NULL
  set = function(y) {
    x <<- y
    y <<- NULL
  }
  get = function() x
  set_inverse = function(inverse) y <<- inverse 
  get_inverse = function() y
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}

cacheSolve <- function(x, ...) {
  y = x$get_inverse()
  if (!is.null(y)){
    message("getting cached data")
    return(y)
  }
  
  mat.data = x$get()
  y = solve(mat.data, ...)
  x$set_inverse(y)
  
  return(y)
}
