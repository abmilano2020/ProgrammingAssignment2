## The following function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){ ## define the argument
      inv <- NULL  
      set <- function(y){ 
            x <<- y       
            inv <<- NULL
      }
      get <- function() {x}  ## define the get function
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The following function computes the inverse of matrix returned by the above makeCacheMatrix.
## If the inverse has already been calculated then the function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}