## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix takes an optional matrix x and caches it in a different environemnt
# the matrix can also be set explicitly after the makeCacheMatrix object has been
# instantiated - using the "setter" "set" - and be retrieved using "get"
# example:
# c = rbind(c(1, -1/4), c(-1/4, 1)) # create matrix
# cached_matrix <- makeCacheMatrix()
# cached_matrix$set(c)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverted) m <<- inverted
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## Write a short comment describing this function
# takes an makeCacheMatrix object as argument
# on first call cacheSolve(cached_tarix)  computes the inverse of the matrix from the 
# makeCacheMatrix object and caches it. On second run it retrieves the inverse from the 
# parent environment: continuing from the example above:
# cacheSolve(cached_matrix)

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinv() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  inv_matrix <- solve(matrix)
  x$setinv(inv_matrix)
  inv_matrix
}
