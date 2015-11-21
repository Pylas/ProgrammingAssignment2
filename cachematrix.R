#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
#create a list of functions that will be used to invert matrix and cache the result 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should 
#retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #get the calculated value from makeCacheMatrix function
  m <- x$getinverse()
 
  #check whether the result isn't null (meaning that the matrix has already been inverted)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if m was not calculated the function will invert the matrix and print out the inverted matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
