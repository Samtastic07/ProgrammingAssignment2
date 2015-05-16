## makeCacheMatrix creates a special matrix and cacheSolve 
## calculates the inverse of the matrix, but if it previously
## calculated, then answer is pulled from the cache

## This function stores a list of 4 functions: set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ##sets inverse as NULL, placeholder for future value
    set <- function(y){ ##changes the input of the function to x
        x <<- y
        inv <<- NULL  ##resets inv to NULL value
    }
    get <- function() x  ##gives the matrix stored in the function
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv) ##stores functions into a list
}


## Determines the inverse of the matrix. If the inverse was previously
## calculated then returns the cached result 
## (using the getinv function from makeCacheMatrix)

cacheSolve <- function(x, ...) {
        inv <- x$getinv() 
        if(!is.null(inv)) {  ##checks to see if inverse already cached
            message("getting cached data")
            return(inv)
        }
        data <- x$get() ##if not cached, computes inverse
        inv <- solve(data, ...) 
        x$setinv(inv)
        inv
}
