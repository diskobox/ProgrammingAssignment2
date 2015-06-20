## Coursera R Programming Assignment 2
# Defines functions makeCacheMatrix and cacheSolve to define a matrix and
# cache its inverse.

## makeChacheMatrix
# For a given matrix, creates an object containing functions to set and 
# reference the original matrix and its inverse.
# Arguments:
# x = invertible matrix
# Returns:
# A list of functions get, set, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
# For a given makeCacheMatrix object, returns the inverse of the matrix.
# Arguments:
# x = makeCacheMatrix object 
# Returns:
# Inverse of the matrix in x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

mtrx <- matrix(rnorm(25), 5, 5)
a <- makeCacheMatrix(mtrx)
a_inv <- cacheSolve(a)
a$get()
solve(a_inv)

