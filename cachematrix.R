## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  cachedInv <- NULL 
  set <- function(input = matrix()) 
  {
    x <<- input
    cachedInv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(invmat)
  {
    cachedInv <<- invmat 
    return(cachedInv)
  }
  getInverse  <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) 
{ 
  calculatedInverse <- x$getInverse() 
  
  ##check if there's a cached solved matrix
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) 
  { 
    message("Cached data found")
    return(calculatedInverse)
  }
  
  matrixToSolve <- x$get()  
  calculatedInverse <-  solve(matrixToSolve)
  x$setInverse(calculatedInverse)
}
