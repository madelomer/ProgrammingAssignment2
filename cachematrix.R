## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(solveMatrix) i <<- solveMatrix
  getInv <- function() i
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInv(i)
  i
}
mt <- matrix(1:4, nrow = 2, ncol = 2)
cacheSolve(makeCacheMatrix(mt))