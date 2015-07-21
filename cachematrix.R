## Coursera Course: R Programming
## programming Assignment 2
## Author: Jigme Norbu.




## Below are two functions that are used to create a special object that stores 
## a matrix and caches its inverse.


## 1. makeCacheMatrix():
## This function creates a special "matrix" object that can cache its inverse.
## The first function, makeCacheMatrix creates a special "matrix", which is really a 
## list containing a function to: 

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix<- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) I <<- solve
  getsolve <- function() I
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve= getsolve)
}



## 2. cacheSolve():

## This function calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
## the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  I <- x$getsolve()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setsolve(I)
  I
}





## Example:
## lets put the functions to the test:

## create an invertible matrix "m" and cache its inverse by using the makeCacheMatrix() function 
## which takes a matrix as its argument. Assign it to "cm"

m <-matrix(c(3,4,1,2),2,2)
cm <- makeCacheMatrix(m)

## then we can use the cacheSolve() function that(takes in the output of the first func. "cm"
## as the argument).
cacheSolve(cm)

## Now, the second time you call the function with the same argument you see that it gets the 
## cached value instead of calculating it.
cacheSolve(cm)






