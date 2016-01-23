## These functions give the ability to calculate the inverse of a matrix, and 
## then cache the calculated inverse so it can be recalled without recalculation
## until the matrix changes

## function that creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## computes the inverse of the special "matrix" created by makeCacheMatrix()
## if the inverse has already been created and matrix hasn't changed, then 
## cacheSolve will used the cached matrix instead of recomputing the inverse
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
