## makeCacheMatrix() and cacheSolve() are a pair of functions
## that solve for and store the inverse of a matrix.

## makeCacheMatrix provides functions to store and retrieve matrix x and its inverse.
## For a newly created matrix, its default inverse is set to NULL.
## The return value of makeCacheMatrix is a named list of all the functions
## that can be accessed by a new instance makeCacheMatrix object.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function()
    x
  
  setinverse <- function(i) 
    inv <<- i
  
  getinverse <- function()
    inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve solves for the inverse of a matrix x of type makeCacheMatrix.
## If its inverse has already been solved (i.e., the getInverse() function does not return NULL),
## then the inverse is retrieved from memory instead of solving for the inverse again.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if (!is.null(inverse)) {
    message("Retrieving the inverse...")
    return(inverse)
  }
  
  message("Solving for the inverse...")
  minverse <- solve(x$get())
  x$setinverse(minverse)
  
  minverse
}
