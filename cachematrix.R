## This function creates a special "matrix" object that can cache its inverse
## This function gets & sets matrix and sets and gets inverse value
## <<- operator is used to assign a value to an object in an environment that is
## different from the current environment

makeCacheMatrix <- function(x = matrix()) {
  
      inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## This function retrives the inverse from the cache if the inverse has already calculated and have not changed.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
        inv <- x$getinv()
        
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        
        data <- x$get()
      
        inv <- solve(data, ...) ## solve function is used to compute the inverse of the square martix
        
        x$setinv(inv)
        
        inv ## returns the results
  
}
