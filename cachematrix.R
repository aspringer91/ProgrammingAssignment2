## Put comments here that give an overall description of what your
## functions do

## Description: This function takes a matrix as input and creates an 
## environment with functions that can be called on that matrix.
## The functions are set, get, get_inverse, set_inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y){
      x <<- y
      inv <<- NULL 
    }
    get <- function() x
    get_inverse <- function() inv
    set_inverse <- function(inverse) inv <<- inverse
    list(get = get, set = set, 
         get_inverse = get_inverse, 
         set_inverse = set_inverse)
}


## This function takes a cacheMatirx vector as input and returns the
## inverse of the matrix of output. If the inverse has already been
## calculated it returns the stored value, otherwise it calculates
## the inverse.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  
      inv <- x$get_inverse()
      if(!is.null(inv)) {
        print("Getting cached inverse")
        return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$set_inverse(inv)
      inv
}


