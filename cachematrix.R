## Put comments here that give an overall description of what your
## functions do
## The included functions are for creating and using an inverse (inv) of a matrix
## makeCacheMatrix contains functions to maintain cache
## cacheSolve is called to inspect a cache and 
## use cache or calculate inverse matrix and set the cache

## Write a short comment describing this function
## MakeCacheMatrix is a list of functions to set/get a cached value
## into an object that is scoped outside of the function

makeCacheMatrix <- function(x = matrix()) {
     ## Input
     ## x = a matrix that can be inversed
     ## 
     ## result
     ## a list containing functions to set/get the matrix and its inverse
     
     inv <- NULL
     
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse 
     getinv <- function() inv
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
## cacheSolve checks if a Matrix's inverse has been cached, 
## if inverse matrix is cached (inv == is.null) then cacheSolve returns it 
## if inverse matrix is not cached (inv != is.null), then cacheSolve calls makeCacheMatrix to get inverse Matrix

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     ## get the cached inverse matrix
     inv = x$getinv()
     
     # if the inverse matrix is not null it has been cached
     if (!is.null(inv)){
          ## notify user that cache exists
          message("getting cached data")
          
          ## Exit function, returning the cached value
          return(inv)
     }
     
     ## Cache was null
     ## Get matrix
     matrix = x$get()
     
     ##calculate inverse matrixdoi()
     invmatrix = solve(matrix, ...)
     
     ## cache the inverse matrix for future calls
     x$setinv(invmatrix)
     
     ## return the inverse matrix
     return(invmatrix)
}