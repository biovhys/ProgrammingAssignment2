## This script defines a pair of functions to cache the inverse of a matrix.
## The makeCacheMatrix function creates a special "matrix" object that can store 
## the matrix and its inverse. The cacheSolve function computes the inverse of 
## the special "matrix" object created by makeCacheMatrix, caching the result 
## for future retrieval.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to set and get the matrix, as well as set and 
## get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Invalidate the cache when the matrix is changed
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Attempt to get the cached inverse
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse, cache it, and return it
  mat <- x$get()  # Get the matrix from the object
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the computed inverse
}
