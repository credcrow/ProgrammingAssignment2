## makeCacheMatrix creates a special "matrix" which is a list containing a function to:
## 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix
##

makeCacheMatrix <- function(x = matrix()){
  inverse.m <- NULL  
  
  set <- function(y.matrix){
    x <<- y.matrix
    inverse.m <<- NULL
  }
  
  get <- function(){ 
    # outputs the x.matrix
    x
  }
  set.inverse <- function(inverse){
    # defines the inverse.m of matrix x
    inverse.m <<- inverse
  } 
  get.inverse <- function(){
    # outputs the inverse of the x.matrix  
    inverse.m
  }
  
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
  
}


## cacheSolve returns the inverse of a matrix and
## stores this value in makeCacheMatrix.  If the 
## inverse matrix has already been calculated it
## returns the cached value.

cacheSolve <- function(x, ...){
  inverse.m <- x$get.inverse()
  
  if(!is.null(inverse.m)){
    message("getting cached data")
    return(inverse.m)
  }
  
  data <- x$get()
  
  inverse.m <- solve(data, ...)
  
  x$set.inverse(inverse.m)
  
  inverse.m
}
