##
## This program provides two functions: makeCacheMatrix and cacheSolve to 
## create a special "matrix" object that can cache its inverse.
## 
##
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.
##  Computing the inverse of a square matrix can be done with the solve function in R. 
##
##  For example, if X is a square invertible matrix, then solve(X) returns its inverse.
##  
##  Example:
##
##  > A <- makeCacheMatrix (matrix(1:4, 2,2))
##  > B <- cacheSolve(A)
##  > B
##  [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

##----------------------------------------------------------------------------------
## makeCacheMatrix: 
## 
## This function creates a special "matrix" object that can cache its inverse.
## returns matrix that can cache its data and inverse
##-------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
 
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  
  setsolve <- function(solve) s <<- solve
  
  getsolve <- function() s
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


##---------------------------------------------------------------------------------------
## cacheSolve: 
##
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.
##
## Return a matrix that is the inverse of 'x'
##---------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  
  
  s <- x$getsolve()
  
  ##----------------------------
  ## check for inverse from cache
  ##----------------------------
  
  if(!is.null(s)) {
    message("getting inverse from cached data")
    return(s)
  }
  
  
  
  ##-----------------------------------------------------
  ## if not in cache, then compute inverse using 'solve'
  ##-----------------------------------------------------
 
  
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)

  s
}
