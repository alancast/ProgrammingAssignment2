## 1. makeCacheMatrix is used to create a special "matrix"
## this special "matrix" has the benefits of cacheing to potentially save time
## 2. cacheSolve returns the inverse of the special "matrix" passed in
## it checks if it has already been computed and cached to save operations

## makeCacheMatric creates a special "matrix" from the passed in value x
## if there is no x passed in the function will create an empty matrix
## the special "matrix" created is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    ## the inverse of the matrix to be stored
    inv <- NULL
    set <- function(y) {
        x <<- y
        ## x has changed so the inverse must be reset to null
        ## and be recalculated
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve is a function to find the inverse of the matrix x passed in
## it first checks if the inverse has already been solved and is cached
## if it is cached it returns the cached value and saves the computation
## if not it computes the inverse and returns that value
cacheSolve <- function(x, ...) {
    ## try to get the inverse of x
    inv <- x$getInverse()
    ## if it's not null it's already been calcuated so take cached value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if the value we got was null we will need to compute the inverse here
    data <- x$get()
    ## compute the inverse
    inv <- solve(data, ...)
    ## set inverse so that next time it could be gotten from cache
    x$setInverse(inv)
    inv
}
