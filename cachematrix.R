## These function construct a list of functions to preform cached 
## operations on a matrix and use those functions to calculate the inverse
## of the matrix

## This function builds the matrix interface
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function uses the matrix interface to return the inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if (!is.null(inv)) return(inv)
  
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}
