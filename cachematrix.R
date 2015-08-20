## This function computes the inverse of a matrix and caches it. When the same calculation is done just before it gets the results
## from the cache instead of computing it again

## This function creates a list containing 4 functions
## 1.Set a matrix
## 2.Get the matrix
## 3.Set the inverse of the matrix
## 4.Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the matrix. However, it first checks if the inverse has already been computed.
## If that is the case it gets the results from the cache and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}