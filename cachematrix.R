## This file contains two functions makeCacheMatrix and cacheSolve.These two functions compute and cache 
## the inverse of a matrix.  
## The makeCacheMatrix function takes advantage of the scoping rules to preserve the state of the object.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will
## retrieve the inverse from the cache.


## The makeCacheMatrix function creates This function creates a special "matrix" object that can 
## cache its inverse. This function also has inner function calls that allow maniputation of cache.
## The underlying implemetation takes advantage of the scoping rules of the R language and how they 
## can be manipulated to preserve state inside of an R object
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize the cachedInverse matrix
  cachedInverse <- NULL
  
  # Setter function for matrix 'x'
  setMatrix <- function(y) {
    
    ## Search through parent environments for an existing definition of the variable 'x' being assigned. 
    ## If 'x' is found (and its binding is not locked) then its value is redefined to 'y', otherwise assignment 
    ## takes place in the global environment
    x <<- y
    cachedInverse <<- NULL
  }
  
  # Getter function for  matrix 'x'
  getMatrix <- function(){
    x
  }
  
  # Function to set the inverse of a matrix
  setInverseInCache <- function(inverseValue){
    message("Adding to cache")
    cachedInverse <<- inverseValue
  }
  
  # Function to return the inverse of a matrix
  getInverseFromCache <- function(){
    message("Returning from cache")
    cachedInverse
  }
  
  # Build a list containing the above function calls
  list(getMatrix = getMatrix, 
       setMatrix = setMatrix,
       setInverseInCache=setInverseInCache,
       getInverseFromCache=getInverseFromCache)

}

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve will retrieve the inverse from the cache and returns it.
cacheSolve <- function(x, ...) {
  
  message('in cacheSolve function')
  
  # If the inverse of this matrix 'x' is already in the cache, then just return it
  inversedMatrix <- x$getInverseFromCache()

  if(!is.null(inversedMatrix)){
    # Hit!!
    message("Cache Hit!!")
  }
  else{
    # Miss!!, set the inverse in the cache for next hit
    message("Cache Miss!! Calculating the inverse")
    inputMatrix <- x$getMatrix()
    ## Determine the inverse
    inversedMatrix <- solve(inputMatrix)
    ## Set the value in cache for future invocations
    x$setInverseInCache(inversedMatrix)
  }
  inversedMatrix
}
