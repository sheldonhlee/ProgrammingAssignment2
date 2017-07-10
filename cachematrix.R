## These two functions are used to compute a matrix inverse and store
## the result to memory. 

## makeCacheMatrix accepts an invertible matrix and saves it as a special
## matrix that can be used later. It contains a list of 4 functions:
## set() is used to set the value of the vector
## get() is used to retrieve the value of the vector
## setinverse() is used to compute the matrix inverse
## getinverse() is used to retrieve a matrix inverse that was previously computed

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set = function(y)
  {
    x <<- y
    m <<- NULL
  }
  get = function() x
  setinverse = function(solve) m<<- solve
  getinverse = function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}


## cacheSolve takes in the special matrix that was created by use
## of the makeCacheMatrix function, and computes its inverse. If the
## inverse was already computed, it retrieves it by calling the getinverse() 
## function. If not, it calls solve() to compute the matrix inverse.
## In both cases, it returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m = x$getinverse()
  if(!is.null(m))
  {
    message('getting cached data')
    return(m)
  }
  data = x$get()
  m = solve(data,...)
  x$setinverse(m)
  m
}
