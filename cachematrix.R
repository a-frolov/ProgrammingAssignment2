## This (makeCacheMatrix) function creates a special "matrix" object
## that can cache its inverse

## As an argument the function uses a n X n invertible matrix that can be created
## in the prompt. Ex.: matrix1 <- matrix(1:4, 2, 2)
##                  matrix2 <- matrix(c(3, -2, 2, -1), 2, 2)

## The function can be called then as cacheMatrix1 <- makeCacheMatrix(matrix1), 
## or cacheMatrix2 <- makeCacheMatrix(matrix2), where cacheMatrix1 and cacheMatrix2
## can be in turn used as arguments for cacheSolve function.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv<- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This(cacheSolve) function calculates the inverse of the matrix 
## returned by makeCacheMatrix function. 
## In case of the inverse being already calculated and the matrix being unchanged,
## it returns the cached value of the inverse.

## The function can be called as cacheSolve(cacheMatrix1) or cacheSolve(cacheMatrix2), 
## where cacheMatrix1 and cacheMatrix2 are the results of makeCacheMatrix 
## function execution.

cacheSolve <- function(x, ...) {

  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
