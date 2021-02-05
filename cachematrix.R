## Put comments here that give an overall description of what your
## functions do

## Purpose:
## The functions contained in this file are designed to provide a caching
## matrix that will store the result of the solve function.
## By caching the result, especially for larger matrices, the amount
## of computation required can be reduced should the result
## be needed multiple times.

## Usage:
## Creating an instance of a caching matrix object using the default value
## > myCacheMatrix <- makeCacheMatrix() #Returns an empty caching matrix
## An instance can also be created using an existing matrix provided by the user
## > myCacheMatrix <- makeCacheMatrix(matrix(rnorm(9), nrow=3, ncol=3))
## Assign a new value to an existing caching matrix
## > myCacheMatrix$set(matrix(rnorm(100), nrow=10, ncol=10))
## Retrieve the matrix stored within a caching matrix
## > myCacheMatrix$get()
## Get the result of the solve function, returning the cached value if it exists
## > cacheSolve(myCacheMatrix)

## Write a short comment describing this function

## Purpose:
##   Create an instance of a cached matrix, return pointers to the cached matrix
##   functions.
## Input:
##   x (optional) - An instance of a standard matrix.
## Output:
##   A list of pointers to the cached matrix functions.
## Functions:
##   get() - Returns the stored matrix.
##   set(x) - Replaces the stored matrix with the matrix 'x' provided.
##   setsolve(x) - Stores the result of solve on the matrix (used by cacheSolve)
##   getsolve() - Returns the cached result of solve

makeCacheMatrix <- function(storedMatrix = matrix())
{
    ## Initialize the cached value
    cachedValue <- NULL
    
    ## Define the set function. Stores the input, reinitializes the cached value
    set <- function(newMatrix)
    {
        storedMatrix <<- newMatrix
        cachedValue <<- NULL
    }
    
    ## Define the get function. Returns the stored input.
    get <- function()
    {
        storedMatrix
    }
    
    ## Define the setsolve function. Sets the cached value.
    setsolve <- function(solvedValue)
    {
        cachedValue <<- solvedValue
    }
    
    ## Define the getsolve function. Returns the cached value.
    getsolve <- function()
    {
        cachedValue
    }

    ## Return a list of pointers to the caching matrix functions.
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

## Purpose:
##   This function will return the cached value of Solve if it already exists,
##   otherwise compute the value and store it in the cache before returning.
## Input:
##   x - A cached matrix result from calling makeCacheMatrix.
##   ... - Additional parameters intended to be passed to the Solve function.
## Output:
##   The result of the solve function.

cacheSolve <- function(cacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## Do some validation to ensure the value provided is compatible
    if(class(cacheMatrix) != "list" ||
       is.null(cacheMatrix$get) || is.null(cacheMatrix$set) ||
       is.null(cacheMatrix$getsolve) || is.null(cacheMatrix$setsolve))
       {
           stop("An incorrect value type was provided to the method in the first
argument. This method requries a cached matrix which can be created using
the makeCacheMatrix function. More documentation can be found in cachematrix.R")
       }
    
    ## Check to see if a cached value already exists
    cachedValue <- cacheMatrix$getsolve()
    if(!is.null(cachedValue))
    {
        ## Let the user know a cached value was returned, then provide the value
        message("Returning cached value for solve.")
        return(cachedValue)
    }
    else
    {
        ## No cached value for solve exists, so compute solve, cache the value,
        ## then return it to the user.
        data <- cacheMatrix$get()
        value <- solve(data, ...)
        cacheMatrix$setsolve(value)
        return(value)
    }
}
