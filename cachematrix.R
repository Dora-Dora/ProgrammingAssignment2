## The following two functions makeCacheMatrix and cacheSolve are used 
#     to create a special object that stores a numeric matrix and stores
#     its inverse

## makeCacheMatrix creates a special "matrix" object that can cache
#    its inverse. It actually produces a list of functions to
#     1. set the value of the matrix
#     2. get the value of the matrix
#     3. set the value of the inverse
#     4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## cacheSolve calculates the inverse of the special "matrix" created 
#     with the above makeCacheMatrix function. However, it first checks
#     to see if the inverse has already been calculated. If so, it gets
#     the inverse from the cache and skips the computation. Otherwise, 
#     it calculates the inverse of the data and sets the value of the
#     inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
