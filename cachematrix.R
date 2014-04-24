## Put comments here that give an overall description of what your
## functions do

## This function creates a "special" cached matrix that contains functions that will
## set the value of the matrix, get the matrix, set the value of the inverse of the
## matrix, and then get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function takes the cached matrix that was created using the 
## makeCacheMatrix function, and produces the inverse of this matrix. If the inverse
## has previously been calculated, the function prints "getting cached data" and the
## inverse matrix. If the matrix has not yet been calculated, it calculates it and
## prints the inverse matrix.

  cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }

## Return a matrix that is the inverse of 'x'