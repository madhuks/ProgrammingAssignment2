## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly

## Following functions are used to cache the inverse of a Matrix

## makeCacheMatrix function creates a list containing function to 
## to set the value of the matrix
## to get the value of the matrix
## to set the value of the inverse
## to get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The function Cachesolve calculates the inverse of a Matrix. It first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## How to run this program? (Sample run)
##> Y <- matrix(c(7,2,3,2,5,6,3,6,9),nrow=3, ncol=3)
##> y1 <- makeCacheMatrix(Y)
##> cacheSolve(y1)
##[,1]          [,2]        [,3]
##[1,]  1.666667e-01  6.344132e-17 -0.05555556
##[2,]  3.238150e-17  1.000000e+00 -0.66666667
##[3,] -5.555556e-02 -6.666667e-01  0.57407407
##> cacheSolve(y1)
##getting cached data
##[,1]          [,2]        [,3]
##[1,]  1.666667e-01  6.344132e-17 -0.05555556
##[2,]  3.238150e-17  1.000000e+00 -0.66666667
##[3,] -5.555556e-02 -6.666667e-01  0.57407407

