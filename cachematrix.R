## Put comments here that give an overall description of what your
## functions do

## Same logic as example, makeCacheMatrix creates a list of functions: 
## set(), get(), setsolve() and getsolve()
## makeCacheMatrix creates a matrix object, either by calling the function with 
## y <- makeCacheMatrix(x), where x is a valid invertible matrix object or, 
## y <- makeCacheMatrix() and then calling y$set(x), where x is a valid invertible matrix object



## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Write a short comment describing this function

## cacheSolve uses solve function to invert an invertible matrix created 
## with makeCacheMatrix function. Usage: 
## cacheSolve(y), where y is an object created with makeCacheMatrix function.
## If the inverse of the matrix had already been calculated, then cacheSolve function
## will use inverse matrix stored in "cache", otherwise will invoke solve function to 
## calculate the inverse and store it in "cache" variable.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}
