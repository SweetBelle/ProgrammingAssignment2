
## Coursera Week 3 Peer-Graded Assignment in R ##

## The function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#  get the value of the matrix, set the inverse Matrix and get the inverse Matrix.
#  The Matrix object can cache its own object.

makeCacheMatrix <- function(x = matrix()) { 
  invMat <- NULL
  setMat <- function(y) {                 # (set the value of the Matrix)                
    x <<- y                               # (<<- is used to assign a value to an
                                          # object in an environment that is different
                                          # from the current environment)
      invMatrix <<- NULL
  }
  getMat <- function() x                            # (get the value of the Matrix)
  setInv <- function(inverse) invMat <<- inverse    # (set the value of the
                                                    # invertible matrix)
  getInv <- function() invMat                       # (get the value of the 
                                                    # invertible matrix)
  list(setMatrix = setMat, getMatrix = getMat, 
       setInverse = setInv, getInverse = getInv)
}


## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix)
#  as an input and checks inverse matrix form makeCacheMatrix(matrix) has any value
#  in it or not.
#  In case inverse matrix from makecacheMatrix(matrix) is empty, it gets the original
#  matrix data from and set the invertible matrix by using the solve function.

cacheSolve <- function(x, ...) {   
  ## Return a matrix that is the inverse of 'x' ##
  invMat <- x$getInv()
  if(!is.null(invMat)) {                            # (if inverse matrix is not NULL, then
    message ("Getting Cached Invertible Matrix")    #  the message "Getting Cached Invertible Matrix"
                                                    #  will appear)
    return(invMat)                                  # (return the invertible matrix)
  }  
  ## if the value of the Invertible Matrix is NULL then ##
  MatData <- x$getMat()                             # (get the original Matrix data)
  invMat <- solve(MatData, ...)                     # (use the solve function to inverse
                                                    # the matrix)
  x$setInv(invMat)      
  return(invMat)
}

