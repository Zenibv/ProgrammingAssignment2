## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Library MASS is to calculate the inverse of both squared and non squared matrixes
## makeCacheMatrix includes set,get,setinv,getinv
library(MASS) 
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


## Write a short comment describing this function
## This is for getting the cach data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
