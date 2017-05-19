## The below functions, makeCacheMatrix and cachesolve together allow
## the inverse of a matrix to be calculated and stored in the cache for
## later use. This is anticipated to be of use when dealing with large
## matrices whose inverse is used repeatedly, as calculating an inverse
## is computationally expensive.

## makeCacheMatrix stores a matrix in a cache, stores the inverse
## when cacheSolve is called, and defines 2 setter and 2 getter functions
## to enable the matrix and inverse to be modified and read

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


## cacheSolve checks whether an inverse matrix is stored in the 
## cache. If so, it returns this cached data. If not, it calculates 
## the inverse and writes it to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
