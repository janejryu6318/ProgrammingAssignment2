## Following two functions are used to cache and inverse a matrix
## makeCacheMatrix function returns value of inverse of the matrix, while cacheSolve function returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) { #set the value of the matrix
            x <<- y
            m <<- NULL
      }
      get <- function() x #get the value of the matrix
      setcache <- function(solve) m <<- solve
      getcache <- function() m
      list(set = set, get = get,
           setcache = setcache,
           getcache = getcache)
}

cacheSolve <- function(x, ...) {
      m <- x$getcache()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setcache(m)
}
