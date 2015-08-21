## Put comments here that give an overall description of what your
## functions do
## These two functions work in sequence: makeCache stores a
## matrix and creates 4 methods to modify that cached matrix.
## cacheSolve returns the inverse of the cached matrix


## Write a short comment describing this function
## makeCacheMatrix takes a square matrix as input 
## and creates an object that stores a matrix and four methods

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function
## this function takes the "CacheMatrix" object created in makeCacheMatrix
## and returns the inverse of the stored matrix by using the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
