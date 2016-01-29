## In this programming assignment I will 
##  write an R function  to cache 
## potentially time-consuming computations.

## An example of time-consuming computation is matrix inversion. And it makes it easier than
## calculating the matix repeatedly is to catch the inverse of a matix. 
## I will write two fuctions that will creat special object to store a matix 
## and caches its inverse.

## Now make a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 

## Return a matrix that is the inverse of 'x'

     inv <- x$getInverse()
     if (!is.null(inv)) {
         message("getting cached data")
         return(inv)
     }
     mat <- x$get()
     inv <- solve(mat, ...)
     x$setInverse(inv)
     inv
}

 ## Sample run:
 x = rbind(c(1, -1/4), c(-1/4, 1))
 m = makeCacheMatrix(x)
 m$get()
      [,1]  [,2]
[1,]  1.00 -0.25
[2,] -0.25  1.00
