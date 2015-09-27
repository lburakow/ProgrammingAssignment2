## Goal: to cache potentially time-consuming computations
## These functions create a matrix first and then cache the inverse of a matrix instead of computing it repeatedly


## This function creates a special "matrix" object that can cache its inverse

## The numbered lines detail the things I did running/testing this code
## 1. I first ran invertme <- makeCacheMatrix()

makeCacheMatrix <- function(x = matrix, ...) { 
        
        inv <- NULL                                                   ## initialize the inverse to null
        
        set <- function(y){                                           ## set the matrix = x in the working environment
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x                                           ## get the matrix
        setinverse <- function(inverse) inv <<- inverse                   ## set the inverse matrix to be the inverse of matrix x and store it in inv (cache)
        getinverse <- function () inv                                     ## get the inverse matrix from where we stored it in the cache 

        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## list functions; return them
        
}

## This function computes and returns the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache

## 2. Then I ran invertme$set(matrix(1:4, 2, 2)) to create my matrix
## 3. Next, I ran cacheSolve(invertme) (output below)

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()                                      ## check for existing inverse of x
        if(!is.null(inv) && identical(x, x$get())) {               ## if the inverse exists (and is for the same matrix as the 
                                                                   ## makeCacheMatrix), display message and then return inverse
                message("getting cached data")
                return(inv)
                
        }
        
        data <- x$get()                   ## create a matrix to take the inverse of
        inv <- solve(data, ...)           ## take the inverse! huzza!
        x$setinverse(inv)                 ## cache the inverse
        inv                               ## return the inverse
        
}

## 4. This is the output I got after running steps 1-3, and repeating 3 once.
## > cacheSolve(invertme)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(invertme)
## getting cached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5