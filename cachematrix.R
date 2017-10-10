## 10-Oct-2017
## R Programming - Week 3: Programming Assignment 2: Lexical Scoping
## Pair of functions that cache the inverse of a matrix.
## To run these functions ... 
## 1. Construct an invertible matrix, say m1
## 2. Call makeCacheMatrix as m2 <- makeCacheMatrix (m1)
## 3. Verify caching by calling cacheSolve (m2) more than once

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) m <<- solve
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix ()
  if(!is.null(m)) {
    message("The matrix is already inverted. Getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve (data, ...)
  x$setInverseMatrix(m)
  m
}
