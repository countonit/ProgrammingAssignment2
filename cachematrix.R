## The functions contained in the file allow a user to retrieve a cached inverse of a matrix 
## rather than having to run the inverse function each time that R needs it. This is especially useful if the
## inverse is needed many times like in a loop function.

## This function creates a vector that is used by the second function. It takes a matrix as an argument then
## creates four functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second functions checks to see if we already have the inverse of the function in cache.
## If so it will return that inverse from the cache as well as printing a message to the console
## letting us know that it was able to retrieve it from cache, if not it will calculate the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## This function is somewhat difficult to test but I ran the following to test 
##it: cacheSolve(x<-makeCacheMatrix(matrix(1:4, ncol=2)))
