## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    inverse <<- NULL
    
    # use `<<-` to assign a value to an object in an environment 
    
    # different from the current environment. 
  
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inverse <<- inverse
  
  getinverse <- function() inverse
  
  list(set = set, get = get,
       
       setinverse = setinverse,
       
       getinverse = getinverse)

}



cacheSolve <- function(x, ...) {
  
  ## x is the output of makeCacheMatrix()
  
  ## cacheSolve() will return inverse of the original matrix input to makeCacheMatrix()
  
  inverse <- x$getinverse()
  
  # if the inverse has already there
  
  if (!is.null(inverse)){
    
    # get inverse from the cache without compute inverse again. 
    
    message("getting cached data")
    
    return(inverse)
  }
  
  # otherwise, compute the inverse 
  
  data = x$get()
  
  inverse <- solve(data, ...)
  
  x$setinverse(inverse)
  
  inverse
}

