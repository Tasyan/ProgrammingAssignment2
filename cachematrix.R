##Here is presented an R function that is able to cache some time-consuming computations such as matrix inversion. If the contents of a matrix are not changing, we can cache the inverse matrix so that when we need it again, it can be looked up in the cache rather than recomputed. 


##This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. Otherwise, it calculates the inverse of the matrix and sets the value of it in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
