## This function makes a cache object with the list to get,set matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
## First time without call to cacheSolve, Initiate inverse of matrix to NULL  
  inv <- NULL
## Set matrix x
  set <-function(y){
    x<<-y
    inv<<-NULL
  }
## Get matrix x
  get <-function() x
## Set inverse of matrix to inv
  setinv <- function(I) inv <<- I
## Get inverse of matrix inv
  getinv <- function() inv
## Return list of functions to act on object
   list(set = set,get =get, setinv = setinv,getinv =getinv)
}


## This function tries to fetch inverse from the previouly created object
## If it can not find the object then it caclulate the inverse and set the inverse of given object
## The output of this function is inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Try to fetch inverse from the object
  inv <- x$getinv()
  ## If inverse is found return the inverse value 
  if(!is.null(inv))
  {
    message("Getting cached inverse")
    return(inv)
  }
  ## if inverse is not found from the object extract data and calculate inverse
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
