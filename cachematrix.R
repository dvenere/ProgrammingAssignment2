## The first fuction create a object based on a matrix, bu
## t it also has a variable to cache its inverse matrix, a
## nd provides the function to obtain this inverse matrix.

## The cachesolve function evaluate if the previously created matrix
## doesnt check if it has already been calculated its inve
## rsed matrix, if it does exist then the inv matrix is re
## turned. If the inverse matrix has not been calculated t
## hen is calculated.

## Create a list of this special matrix with function to
## obtain its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



##Calculate the inverse matrix. Returns the inverse ma
##trix cached if it has already been calculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting cached inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
