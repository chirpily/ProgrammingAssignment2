## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly (there are also alternatives to matrix inversion that we will not 
## discuss here). Your assignment is to write a pair of functions that 
## cache the inverse of a matrix.

## This file includes the second programming assignment for the R programming on Coursera

## In this makeCacheMatrix function, makeCacheMatrix creates a special "Matrix",
## which is a matrix that contains a function to

## First. Set the value of the matrix
## Second. Get the value of the matrix
## Third. Set the value of the inverse of the matrix
## Fourth. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) x_inverse <<- inverse
  get_inverse <- function() x_inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## Function to return matrix inverse from cache if it exist
## if not available calculate using cachesove()

cachesolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}

