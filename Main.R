makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
makeVector(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

cachemean(makeVector(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)))

