## This function is able to cache the potentially time-consuming
## computation of matrix inversion. 
## 

## This function creates a special "matrix" object that can cache its inverse

##makeCacheMatrix takes a matrix as input 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  ##set the value of the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x       ##get the value of the matrix
  set_inv <- function(solve) m<<- solve     ##set the value of the inverse
  get_inv <- function() m    ## get the value of the inverse of the matrix
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv()
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inv(m)
  return(m)
}
