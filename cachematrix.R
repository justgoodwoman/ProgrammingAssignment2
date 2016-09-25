##makeCacheMatrix - function creates a special "matrix" object that can cache its inverse.

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

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
          
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
