## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    ## Sets the value of the matrix
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## Gets the value of the matrix
    get <- function() x
    
    ## Sets the value of the matrix inverse
    setinverse <- function(solve) m <<- solve
    
    ## Gets the inverse of the matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Retrieve the function - if value present, it is getting cached data
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## This part gets executed first time the function is called
  data <- x$get()
  m <- solve(data, ...)
  
  ## Caching result
  x$setinverse(m)
  m
}
