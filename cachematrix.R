## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## The makeCacheMatrix function, takes in a matrix as a parameter
## and returns a list of functions which does the following 
## operations:
## 1. getMatrix() - Returns the original matrix
## 2. setMatrix() - Sets the original matrix to a new matrix
## 3. setRevMatrix() - Sets the inverse of the original matrix
## 4. getRevMatrix() - Returns the reverse of the original passed matrix
makeCacheMatrix <- function(x = matrix()) {
   rev_m <- NULL
   setMatrix <- function(y) {
     x <<- y
     rev_m <<- NULL
   }
   getMatrix <- function() x
   setRevMatrix <- function(reversed) rev_m <<- reversed
   getRevMatrix <- function() rev_m   
   list(setMatrix = setMatrix, getMatrix = getMatrix,
        setRevMatrix = setRevMatrix,
        getRevMatrix = getRevMatrix)
}


## The cacheSolve function, takes in as parameter an object
## of type makeCacheMatrix and returns the inverse of the 
## matrix of the passed object if it is already cached else, 
## computes the inverse of the matrix, sets the inversed matrix
## in cache and returns the inversed matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matx <- x$getRevMatrix()
  if(!is.null(matx)) {
    message("getting cached matrix")
    return(matx)
  }
  orig_matx <- x$getMatrix()
  matx <- solve(orig_matx)
  x$setRevMatrix(matx)
  matx
}
