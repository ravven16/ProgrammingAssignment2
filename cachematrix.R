## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix -> The function gets a matrix as an input parameter
## If no input is provided an empty matrix is passed as default
## The function have get and set function for matrix creation and retrieval
##Also, it has get and set function for retrieving the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) { 
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <-function() x
        setInverse <- function(invinput) inverse <<- invinput
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
        
}


## Write a short comment describing this function

## cacheSolve -> The function is used to obtain the inverse from 
## makeCacheMatrix function, if it is already computed. If the inverse
## is not calculated earlier, the function will compute the inverse
## It reduces the unnecessary computation of inverse, when it is already
## computed, hence it minimizes the CPU usage. 

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("getting from cached data")
                return(inverse)
        }
        out <- x$get()
        inverse <- solve(out)
        x$setInverse(inverse)
        inverse
} 