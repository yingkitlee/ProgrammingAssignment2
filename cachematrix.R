## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Create special matrix object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {

	## Initialize the inverse property
        inv <- NULL
        
        ## Set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the matrix
        get <- function() x
        
        ## Set the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse
        
        ## Get the inverse of the matrix
        getInverse <- function() inv
        
        ## Return the list of the methods
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated, inverse will be retrieved from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    ## Return inverse if its already set
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Get the matrix from object
    mat <- x$get()
    
    ## Computes the inverse
    inv <- solve(mat, ...)
    
    ## Set the inverse to the object
    x$setInverse(inv)
    
    ## Return the matrix
    inv
}
