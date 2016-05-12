## As per ProgrammingAssignment2 these functions are used
## to create a special type of inverse cachable matrix
## and provide a mechanism to obtain the cached value

## This function creates a matrix type object that is 
## capable of caching its inverse for faster access

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function is similar to solve, except it operates
## on an object of the type described above in order to 
## make use of the cached inverse if it is available.
## If the inverse is not cached, it will compute the inverse
## and then store it in the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("cached value returned")
    return(i)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
