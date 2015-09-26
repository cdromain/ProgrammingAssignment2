## R Programming - Week 3 - Programming Assignment 2
##
## This code file contains 2 functions, makeCacheMatrix and cacheSolve,
## which are meant to be used the following way :
##
## 1. create a matrix object, for example : x <- matrix(rnorm(100, 1), 10, 10)
## 2. list <- makeCacheMatrix(x)
## 3. cacheSolve(list)

## The makeCacheMatrix function creates & returns a special list containing 4 functions
##
## 1. set : sets the content of the matrix which we'll calculate the inverse of
## 2. get : gets the content of the matrix
## 3. setInv : sets the cached value of the matrix inverse
## 4. getInv : gets the cached value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL    # inverse variable
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inv) i <<- inv
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## The cacheSolve function computes and returns a matrix that is the inverse of 'x'
## but only if the inverse has not already been calculated (i.e. i = NULL)
## or if the matrix has changed since the last inverse computation (which sets i as NULL)
## otherwise the function retrieves the cached inverse value using the get function

cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) { # this tests if the inverse i has already been computed and if matrix has changed
                message("Cached data available and the matrix has not changed. Getting cached inverse.")
                return(i)
        }
        else message("No cached data available or the matrix has changed. Calculating the inverse and caching it.")
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
        i
}
