## Matrix inversion is usually a costly computation and there may be some benefit to caching the
## inverse of a matrix rather than compute it repeatedly.
## I have implemented a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  
  i <- NULL
  
  set <- function(y){
    m <<- y
    i <<- NULL
  }
  
  get <- function(){
    return(m)
  }
  
  setinverse <- function(inverse){
    i <<- inverse
  }
  
  getinverse <- function(){
    return(i)
  }

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(m) {
        
  i <- m$getinverse()
  
  if(!is.null(i)){
    message("Please wait! Fetching data for you!")
    return(i)
    }
  
  data <- m$get()
  i <- solve(data)
  m$setinverse(i)
  return(i)
  
}