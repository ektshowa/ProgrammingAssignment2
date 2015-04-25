## These functions compute the inverse of a matrix. We assume here that the input
## Matrix is invertible. If the inverse has not been computed yet, the computation
## is done and the value is cached. 


## This function Create and return a list of getters and setters to set the input
## matrix, set its computed inverse, get the input matrix, get its computed inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL;
    
    ## Set the input matrix. 
    set <- function(y){
        x <<- y;
        matrixInverse <<- NULL;
    }
    
    ## Get the input matrix
    get <- function() x;
    
    ## Set the inverse matrix
    setInverse <- function(inverse) matrixInverse <<- inverse;
    
    ## Get the inverse matrix
    getInverse <- function() matrixInverse;
    
    ## Build the list of getters and setters and return it.
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse);

}


## This function compute and return the inverse of the input matrix
## It takes as parameter a list returned by the function makeCacheMatrix()
cacheSolve <- function(x, ...) {
  
  ## Get the cached inverse matrix. Return it if has already been computed.
  matrixInverse <- x$getInverse();
  
  if(!is.null(matrixInverse)){
      message("getting cached data")
      return(matrixInverse);
  }
  
  ## If the inverse matrix has not been computed yet, get the input matrix
  ## and compute the inverse
  data <- x$get();
  matrixInverse <- solve(data);
  
  ## Cache the inverse matrix and return it
  x$setInverse(matrixInverse);
  matrixInverse;    
}
