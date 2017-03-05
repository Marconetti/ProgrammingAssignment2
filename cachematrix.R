## As per assignment, the "matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly." The two functions will calculate the inverse of a matrix and cache it such that a second calculation 
## of the same matrix will return a cached inverse instead of recalculating it.  


## The function makeCacheMatrix creates a special object with 4 functions. 
## 1. Set the matrix value. 
## 2. Get the matrix value. 
## 3. Set the inverse of the matrix. 
## 4. Get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
    ## Initialize the inverse with NULL 
    inv  <- NULL
  
    ## 1. Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## 2. Return matrix 
    get <- function() x
    
    ## 3. Set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
  
    ## 4. Return inverse
    getInverse <- function() inv
    
    # Return object with 4 functions 
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
    
 

## The function cacheSolve returns the inverse of a matrix. Since the calculation 
## of the inverse is a costly computation, it checks whether the value has already
## been calculated and returns the cached value instead of recalculating it. 

cacheSolve <- function(x) {

  ## Obtain the mean from the special object
  inv <- x$getInverse()

  ## If the inverse was already cached, return cached data
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  
  ## If the inverse was not cached, calculate the inverse and cache it 
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  return(inv)
}