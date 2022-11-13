library(MASS)
## make functions to cache the inverse matrix, then create the special matrix object to cach its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
                     x <<- y
                     inv <<- NULL
                     }
  get <- function()x
  setinv <- function(inverse)inv<<-inverse
  getinv <- function() {
                       inver<-ginv(x)
                       inver%*%x
                       }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Calculate the inverse of the special matrix returned by "makeCacheMatrix". The "cachesolve" should retrieve the inverse from the cache if the inverse has already been calculated 

cacheSolve <- function(x, ...)
  {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
