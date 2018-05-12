
## makeCacheMatrix takes a matrix as an argument and stores the value in "special" Inverse variable which is cached

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes a matrix as an argument and returns its inverse if already calculated/cached.
## If not cached before then sets the cache value

cacheSolve <- function(x, ...) {
  inverse<- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse<-solve(data)
  message("setting cached data")
  x$setInverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
