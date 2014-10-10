#############################################################
# Function Name :makeCacheMatrix(x = matrix())              #
# Description:                                              # 
#   This function stores the inverse of matrixes,           #
#   if a matrix already exists in the list,                 #
#   we pass the cached result instead of recomputing it     #
# INPUTS:                                                   #
#  name: x                                                  #
#  type: Matrix                                             #   
#  default: matrix()                                        #
#                                                           #
# OUTPUTS:                                                  #
#  name: anonymous                                          #
#  type: list                                               #
#                                                           #
#############################################################
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

#############################################################
# Function Name :cacheSolve(x,...)                          #
# Description:                                              #
#   This function computes the inverse of matrix,           #
#   but it tries to retrieve pre-computed values            #
#   from a cache if its available                           #
#                                                           #
# INPUTS:                                                   #
#  name: x                                                  #
#  type: Matrix                                             #   
#  default: matrix()                                        #
#                                                           #
# OUTPUTS:                                                  #
#  name: i                                                  #
#  type: matrix                                             #
#                                                           #
#############################################################
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
}
