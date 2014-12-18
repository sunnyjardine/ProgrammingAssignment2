## The functions will take matrix x and calculate the inverse of the matrix. If the inverse of x
## has already been calculated the functions will return the inverse values stored in the cache 
## rather than computing them again.

## The function creates an object (of type list) that stores the matrix x 
## and the inverse of that matrix if it has been calculated previously

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinvx <- function(solve) invx <<- solve
  getinvx <- function() invx
  list(set = set, get = get,
       setinvx = setinvx,
       getinvx = getinvx)
}


## The function accesses the object created by "makeCacheMatrix" 
## and calculates the inverse of matrix x if it is not in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          invx <- x$getinvx()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinvx(invx)
  invx
}
