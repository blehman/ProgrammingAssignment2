## The combination of these two functions create a spcial matrix that can cache its inverse. 

## Write a short comment describing this function
# the makeCacheMatrix function creates a special matrix with an attribute of set, get, and setInverse/getInverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
##  creates the inverse of the special CacheMatrix or else pulls it from cache
cacheSolve <- function(x, ...) {
  m <- x$getInverse()           #query the x vector's cache         
  if(!is.null(m)) {           #if there is a cache
    message("getting cached data") 
    return(m)                #just return the cache, no computation needed
  }
  data <- x$get()             #if there's no cache
  m <- solve(data, ...)        #we actually compute them here
  x$setInverse(m)                #save the result back to x's cache
  m                           #return the result
  ## Return a matrix that is the inverse of 'x'
}
