## The functions take in an input matrix and
## store the inverse of that matrix as found 
## with the solve() function so the result can
## be easily referenced without having to be
## recalculated

## makeCacheMatrix clears the variable i and 
## uses the input matrix to create a list 
## containing the results of the solve() function
## that can be referenced

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinverse <- function(solve)i <<- solve
  getinverse <- function()i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve checks to make sure there is a new input
## then returns the inverse of the input matrix


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
