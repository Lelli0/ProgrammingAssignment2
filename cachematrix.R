## run defined, invertible matrix through function to add the ability to
## cache the inverse once it has been solved
## For example: x<-matrix(rnorm(1:9),3,3)
## z<-makeCacheMatrix(x)
##
## 

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  }


##cacheSolve processes makeCacheMatrix(x) or cacheSolve(z) from example above
## to check if inverse has already been calculated
## if it has not,m is still null from earlier function and cacheSolve will
##solve for the inverse and return it as m
##if already solved, returns a msg and the previously calculated result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
