## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a
## matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix"  
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## x: a square invertible matrix
  i <- NULL
## Set the matrix

    set <- function(y) {
    x <<- y
    i <<- NULL
    }
## Get the matrix
    
  get <- function() x
  ## Set and get the inverse of the matrix
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the "matrix" 
## returned by makeCacheMatrix. If the inverse has already been 
## calculated,it gets the result and skips the computation. If not,
## it computes the inverse, sets the value in the cache via setinverse.

cacheSolve <- function(x, ...) {
## x: output of makeCacheMatrix()
  i <- x$getinverse()
  
## if the inverse has already been calculated
  
  if (!is.null(i)) {
    
## retriev it from the cache and skips the computation. 
    
    message("getting cached inverse matrix data")
    return(i)
  }
## otherwise, calculates the inverse 
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
## sets the value of the inverse in the cache via the setinv
  
    i
}

## Sample execution of the code

mymatrix <- matrix(4:7,2,2)
mymatrix
mymatrixI <- makeCacheMatrix(mymatrix)
cacheSolve(mymatrixI)
cacheSolve(mymatrixI)
