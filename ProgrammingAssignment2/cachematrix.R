## There are 2 functions below namely makeCacheMatrix and cacheSolve. 
## The makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The cacheSolve computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function. If the inverse has already been calculated (and the
## matrix has not changed) then cacheSolve should retrieve the inverse from cache.

## The makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function. If the inverse has already been calculated (and the
## matrix has not changed) then cacheSolve should retrieve the inverse from cache.

cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv       
}
