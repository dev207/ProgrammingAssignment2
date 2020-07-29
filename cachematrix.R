## THe codes below are to find inverse of a matrix as per the assignment.
## "makeCacheMatrix" and "cacheSolve" cache the inverse of a given matrix

## this fucntion makes a "matrix" object that cache the inverse of the input matirx
## input of this function should be a invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## the function below computes the matrix inverse of the special "matrix" returned
## by the previous function (makeCacheMatrix). If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the
##inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m        
}
