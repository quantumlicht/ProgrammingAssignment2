# This function stores the inverse of matrixes, if a matrix already exists in the list,
# we pass the cached result instead of recomputing it

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setmean = setinv,
         getmean = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
  
        ## Return a matrix that is the inverse of 'x'
}
