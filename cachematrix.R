## Put comments here that give an overall description of what your
## functions do

## Creating a cache for the matrix

makeCacheMatrix <- function(x = matirx()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setIn <- function(solvedMat) m <<- solvedMat
  getIn <- function() m
  list(set = set, get = get,
       setIn = setIn,
       getIn = getIn)
}

## Solving the matrix via the function Solve()
## unless the matrix actually exists in the memory

cacheSolve <- function(x, ...) {
  m <- x$getIn()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setIn(m)
  m
}