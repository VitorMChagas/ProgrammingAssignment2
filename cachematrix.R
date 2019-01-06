##Vitor Chagas - R Programming Week 3 Assignment 
## This function can cacheand multiple times 
##the inverse of a matrix, reducing time.
## makeCacheMatrix creates a matrix object that 
##can cache the matrix inverse in itself.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMat <- function(y) {   ## setting the matrix
    x <<- y
    inv <<- NULL
  }
  
  getMat <- function() x
  setInv <- function(inverse) inv <<- inverse 
  getInv <- function() inv  ## getting the invertible matrix
 
   list(setMat = setMat, getMat = getMat,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve calculates the inverse from the returned matrix
##of makeCacheMatrix. If the calculation was correct
##cashSolve will retrieve the reverse

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {         
    message("getting cached data")
    
    return(inv)
  }
  
  MatData <- x$getMat()           #retrieve the matrix
  inv <- solve(MatData, ...)      #solve and invert
  x$setInv(inv)           
 
   return(inv)
}
