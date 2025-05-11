## First, makeCacheMatrix() is created, which creates the place to store 
## the inverse of a matrix. 
## Then cacheSolve() is created, which calculates and stores the inverse.


## The makeCacheMatrix() function creates an object that stores a matrix 
## and can cache its inverse. This object includes four internal functions 

makeCacheMatrix <- function(x = matrix()) {    
  # Defines a function that creates an object that stores a matrix. It´s initially empty
  
  inverse <- NULL  
  # Create a variable to store the cached inverse.It´s initially empty
  
  set <- function(y) {
    # This internal function redefines the original matrix
    x <<- y
    inverse <<- NULL
    # When the matrix is changed, resets the cached inverse to NULL
  }
  
  get <- function() x  
  # Defines a function to return the new matrix
  
  setinverse <- function(inv) inverse <<- inv  
  # Defines a function to store the calculated inverse in cache
  
  getinverse <- function() inverse  
  # Defines a function to return the cached inverse
  
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  # Returns a list with the four internal functions so they can be accessed externally
}

## The cacheSolve() function searches for a saved inverse.
## If it finds it,  returns it with a message.
## If not: it calculates it, stores it, and returns it.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse() ## tries to recover the cached inverse from "inverse" 
  
  if (!is.null(inverse)) {## If the inverse is already cached...
    message("getting cached inverse") ## give this message
    return(inverse) ## and return it directly without recalculating
  }
  
  data <- x$get() ## If no cached inverse, get the matrix from the object
  
  inverse <- solve(data, ...) ## calculates the inverse
  
  x$setinverse(inverse) ## caches that new inverse in x
  
  inverse ## returns the newly calculated inverse
}

