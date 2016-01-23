## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
+     set <- function(y) {
+         x <<- y
+         inv <<- NULL
+     }
+     get <- function() x
+     setInverse <- function(inverse) inv <<- inverse
+     getInverse <- function() inv
+     list(set = set,
+          get = get,
+          setInverse = setInverse,
+          getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
+     inv <- x$getInverse()
+     if (!is.null(inv)) {
+         message("getting cached data")
+         return(inv)
+     }
+     mat <- x$get()
+     inv <- solve(mat, ...)
+     x$setInverse(inv)
+     inv
}
> ## Sample run:
> x = rbind(c(1, -1/4), c(-1/4, 1))
> m = makeCacheMatrix(x)
> m$get()
      [,1]  [,2]
[1,]  1.00 -0.25
[2,] -0.25  1.00
