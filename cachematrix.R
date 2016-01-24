## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  ## set is a function that changes the matrix stored in the main function
  set <- function(y) {
    
    ## The operators <<- and ->> are normally only used in functions, 
    ## and cause a search to made through parent environments for an existing definition of the variable 
    ## being assigned.
    ## If such a variable is found (and its binding is not locked) then its value is redefined, 
    ## otherwise assignment takes place in the global environment.
    ## Note that their semantics differ from that in the S language, but are useful in conjunction 
    ## with the scoping rules of R.
    ## See The R Language Definition manual for further details and examples. 
    x <<- y
    
    ## restores to null the value of the --inverse matrix--,
    ## The new --inverse matrix-- needs to be recalculated through the function cacheSolve.
    m <<- NULL
    
  }

  ## get is a function that returns the matrix x stored in the global environment. Doesn't require any input
  get <- function() x
  
  ## setInverseM is a function that set the inverse matrix in the m variable of the global environment
  setInverseM <- function(inverseMatrix) m <<- inverseMatrix
  
  getInverseM <- function() m
  
  
  list(set = set, get = get,
       setInverseM = setInverseM,
       getInverseM = getInverseM)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getInverseM()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  

  data <- x$get()
  m <- solve(data)
  x$setInverseM(m)
  m
}
