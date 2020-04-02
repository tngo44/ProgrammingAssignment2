## This pair of functions, makeCacheMatrix and cacheSolve, can 
##cache the inverse of a matrix

## Function of makeCacheMatrix:
## Creates a special 'matrix' object that can 
## cache its inverse, represented by the variable 'im'.


makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        setmatrix <- function(y) {      #set value of matrix 
                x <<- y
                im <<- NULL
        }
        getmatrix <- function() x       #get value of matrix
        setinverse <- function(solvematrix) im <<- solvematrix   #set value of inv matrix
        getinverse <- function() im      #get value of inverse
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse,
             getinverse = getinverse)
}


## Function of cacheSolve:
## Computes the inverse of the matrix returned by makeCacheMatrix via solve().
## If the inverse of the matrix has already been calculated, cacheSolve will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        if(!is.null(im)){
                message("Getting cached inverse of matrix")
                return(im)
        }
        matrix <- x$getmatrix()
        im <- solve(matrix, ...)    #use the solve function to find inverse
        x$setinverse(im) 
        im
        
}

