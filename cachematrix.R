## The two functions below are used to create a special "matrix" object  and  cache its inverse.

## 
## This function, makeCacheMatrix creates a vector that contains a list of function to
## 1. create the the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function (y) {
          x <<- y
          m <<- NULL        
   }
   get <- function () x
   setinverse <- function(solve)  m <<- solve
   getinverse <- function() m
   list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" created by the above function.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if (!is.null(m))  {
               message("getting cached data")
               return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}
