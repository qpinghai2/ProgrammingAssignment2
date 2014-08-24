## Script contains two functions: makeCacheMatrix and cacheSolve

## makeCacheMatrix creates a special object that stores a matrix. It returns a list which: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of a square matrix
## 4. get the value of the inverse of a square matrix


makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y){
    
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inverse) i <<- inverse
  
  get_inverse <- function() i
  
  list(set = set, 
       get = get, 
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## cacheSolve returns a matrix that is the inverse of 'x'. 
## It first checks if the inverse has been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of 'x' and 
## sets the value of the inverse in the cache via the set_inverse() function.

cacheSolve <- function(x, ...) {
  
  
  i <- x$get_inverse()
  
  if(!is.null(i)){
    
    message("getting cached data")
    
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data, ...)
  
  x$set_inverse(i)
  
  i  
  
}
