## The code consists of two functions. The first function returns a function list: 
## set matrix(setmatrix), get matrix data(getmatrix), set inversed matrix(setinverse) and get inversed matrix(getinverse). 
## The second function checks if an inversed matrix 
## is calculated for a given matrix. If so, it returns the cached inversed matrix data. If not, it caculate the inversed
## matrix and returns it

## This function is to set/get/store the matrix or inversed matrix data

makeCacheMatrix <- function(x = matrix()) {
  # NULL is set to the initial inversed matrix data
  inv <- NULL 
  
  #this function can set matrix data. If a matrix is set, in another word, changed 
  #by this function, the inversed matrix data is set to be NULL again. 
  setmatrix <- function(y) {
    x<<-y
    inv<<-NULL
  }
  
  #this function can get the matrix data
  getmatrix <- function() x
  
  #this function is to set inversed matrix data
  setinverse <- function(inverse) inv<<-inverse
  
  #this function is to get the inversed matrix data
  getinverse <- function () inv
  
  #a list of functions is returned.
  list(setmatrix=setmatrix, getmatrix=getmatrix,setinverse=setinverse,
       getinverse=getinverse)
}


## This function is to get the cached matrix if it was calculated before or calculate the inverse for a new matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  #Return the cached data if the inversed matrix is not null
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  #If the inversed matrix is not calculted before, calculated the inversed matrix and return it. 
  matrix <- x$getmatrix()
  inv <- solve(matrix)
  x$setinverse
  inv
}
