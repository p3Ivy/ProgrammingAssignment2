## Put comments here that give an overall descriptioin of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<-inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function
## This function calculates the inverse of the matrix created in the last 
## function above. As long as the inverse is calculated,then this function 
## should recall the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse f 'x'
  inv = x$getinv()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}