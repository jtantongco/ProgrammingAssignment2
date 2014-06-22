# Author: Jeremiah Tantongco

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function acts as a constructor for a cache matrix object
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse )
}

## Write a short comment describing this function
## This function solves for the inverse of cache matrix object
## If the inverse has been cached, the function will return that result
## otherwise the inverse of the matrix will be calculated
cacheSolve <- function(x, ...){
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("using cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}

## For test input:

# v <- c(1,2,7,3,3,1,6,3,2)
# v2 <- c(5,2,7,3,3,1,6,3,2)
#  m <- matrix(v,3,3)
#  m2 <- matrix(v2,3,3)

## inverse of m:
##[,1]       [,2]  [,3]
##[1,] -0.0500000  0.0000000  0.15
##[2,] -0.2833333  0.6666667 -0.15
##[3,]  0.3166667 -0.3333333  0.05
## inverse of m2:
##[,1]       [,2]    [,3]
##[1,] -0.0625000  0.0000000  0.1875
##[2,] -0.3541667  0.6666667  0.0625
##[3,]  0.3958333 -0.3333333 -0.1875

#  cMat <- makeCacheMatrix(m)
#  cMat$getinverse()
#  cMat2 <- cacheSolve(cMat)
#  cMat$getinverse()
#  cMat3 <- cacheSolve(cMat)
#  cMat$getinverse()

#  cMat$set(m2)
#  cMat$getinverse()
#  cMat4 <- cacheSolve(cMat)
#  cMat$getinverse()
#  cMat5 <- cacheSolve(cMat)
#  cMat$getinverse()