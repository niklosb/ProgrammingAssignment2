## makeCacheMatrix creates a special matrix object that can cache its inverse
## cacheSolve computes the inverse of the special matrix if it is not cached.
# if it is cached, it simply retturns the value.


# Function creates a list of functions that set and get the value of a matrix
# as well as sets and gets the value of the matrix inverse if it has already
# been calculated. If the inverse has not been calculated, then the function
# returns null
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function returns the inverse of a cacheMatrix object by either getting the inverse
# from the object, or calculating the inverese depending on whether or not it
# has been previously calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}



