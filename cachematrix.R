## Creates inverse of matrix if not previously calculated, and calculates if not present

## Sets up "matrix" like object with ability to cache inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinv <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinv = getinv)
  
}


## Uses matrix-like function above to return cached value if present, and otherwise calculates

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
