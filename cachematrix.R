## Programming Assignment 2 - R Programming (Week 3)
## These two functions are part of completing the second programming 
## assignment for the R Programming course 

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
## Write a short comment describing this function
#

makeCacheMatrix <- function(x = matrix()) {

  # Initially set to NULL
  # Changes when the user sets the value
  inv <- NULL

  
  # set function
  # Sets the matrix itself but not the inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get function
  # Gets the matrix itself but not the inverse
  get <- function() x
  
  # Manually set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinverse <- function() inv
  
  # Encapsulate into a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

  # The cacheSolve function returns the inverse of the matrix. It first checks if
  # the inverse has already been computed. If so, it gets the result and skips the
  # computation. If not, it computes the inverse, sets the value in the cache via
  # setinverse function.
  
  # This function assumes that the matrix is always invertible.

  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Following the same format as the assignment example
    
    # Get the current state of the inverse and see if it
    # has been computed yet
    inv <- x$getinverse()
    
    # If it has...
    if(!is.null(inv)) {
      # Simply return the computed inverse		
      message("Getting cached matrix")
      return(inv)
    }
    
    # If it hasn't...
    # Get the matrix itself
    data <- x$get()
    
    # Find the inverse
    inv <- solve(data, ...)
    
    # Cache this result in the object
    x$setinverse(inv)
    
    # Return this new result
    inv    
  }