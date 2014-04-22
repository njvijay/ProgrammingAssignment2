# title : R progamming Assignment 2
# Author : Vijayakumar Jawaharlal

# This is second programming assignment on R programming. This class is conducted by 
# John Hopkins university via coursera. Intention of the this assignment is, working with
# function. Additionally, cache the value of the matrix inverse so that when we need it again, 
# it can be looked up in the cache rather than recomputed. 

## makeCacheMatrix function is collection of getter setter functions to set and get
# matrix and set and get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(minv) m <<- minv
    
    getinv <- function() m
    
    list (set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve - The following function calculates the mean of the special "matrix" created with the above 
## function. However, it first checks to see if the matrix inverse has already been calculated. 
## If so, it gets the matrix inverse from the cache and skips the computation. 
## Otherwise, it calculates the matrix inverse and sets the value of the mean in the cache via the 
## setinv function.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
        ## Return a matrix that is the inverse of 'x'
}

## Here is how above functions are put to use
## a <- makeCacheMatrix(x=matrix(1:4,2,2))
##  d <- cacheSolve(x=a) 
## Try again: d <- cacheSolve(x=a) -- this should get cached data. Look for print "getting cached data"
## d
## above should print the inverse of the matrix which was assigned for 'a'

