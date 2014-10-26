## Using the following two functions will cache the inverse of a matrix.
## Sample output for how it works:
##>    amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##>    amatrix$get()         # Returns original matrix
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##>   cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##>  amatrix$getinverse()  # Returns matrix inverse
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##>  cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) m <<- solve ## calculate the inverse
   getinverse <- function() m                ## return the inverse
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)             ## makes the functions public
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   if(!is.null(m)) {                        ## check to see if result is cached
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)                          ## calculate the inverse
   m                                        ## return the result
}
