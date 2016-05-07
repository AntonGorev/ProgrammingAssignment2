## makeCacheMatrix(...) creats matrix object which takes an initial matrix as an argument 
##  and stores its inverse matrix
## cacheSolve(...) computes an inverse matrix or takes a cached data if inverse for current 
## initial matrix has already been computed 

# Implenet library with givn() method, to compute inverse of non-square matrices
library(MASS)

## This function creates a special "matrix" object, which contains four methods 
##  to set and retrieve initial and inverse matrices respectively. As an argument 
##  it gets initial matrix. Function returns list of methods.

makeCacheMatrix <- function(x = matrix()) {
  
  # Set the value of inverse matrix as NULL if new object is being created
  inv_matrix <- NULL
  
  # Set a new matrix without creating new matrix object
  setMyMatrix <- function(MyMatrix) {
    x <<- MyMatrix                                   
    inv_matrix <<- NULL                             # Erase old inverse matrix since 
                                                    #  initial matrix has been changed
  }
  
  # Define method to get an initial matrix
  getMyMatrix <- function() x
  
  # Define method to store an inverse matrix
  setMyInverse <- function(my_inv){ 
    inv_matrix <<- my_inv
  }
  
  # Define method to get an inverse matrix
  getMyInverse <- function() inv_matrix
  
  # Return object with access methods 
  list(setMyMatrix = setMyMatrix,
       getMyMatrix = getMyMatrix,
       setMyInverse = setMyInverse,
       getMyInverse = getMyInverse)

}


## The following function takes as an argument special matrix object, 
##  retrieves an initial matrix and computes its inverse matrix, then send 
##  it to makeCacheMatrix(...) to cache it.
##  If inverse is already exists, function retrieves the data from makeCacheMatrix(...)
##  and return it

cacheSolve <- function(x, ...) {
  
        # Retrieve cached inverse matrix
        my_inverse <- x$getMyInverse()
        
        # Check if it exists, if yes - then return it and exit function
        if(!is.null(my_inverse)) {
          message("getting cached data")
          return(my_inverse)
        }
        
        ## If inverse matrix doesn't exist then compute it.
        
        # Check if initial matrix is square-matrix,
        #  if 'TRUE' then use solve(), if not then use ginv() from MASS library
        if(nrow(x$getMyMatrix()) == ncol(x$getMyMatrix())){
          my_inverse <- solve(x$getMyMatrix())
        }else{
          my_inverse <- ginv(x$getMyMatrix())
        }
        x$setMyInverse(my_inverse)                  # Cache inverse matrix
        my_inverse
}
