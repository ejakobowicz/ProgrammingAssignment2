## These functions allows to compute the inverse of a matrix 
## and caches it or to retrieve a cached inverse if available

## This function creates a special "matrix" object that can cache its inverse
## This object is a list with different functions

makeCacheMatrix <- function(x = matrix()) {
	invX <- NULL #invX is where the inverted matrix is stored
      set <- function(y) {
                x <<- y
                invX <<- NULL
	}
	get <- function() x
      setInvX <- function(inv) invX <<- inv
      getInvX <- function() invX

## A list with the different function is created
        list(set = set, get = get,
             setInvX = setInvX,
             getInvX = getInvX)	
}


## This function will calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the mean has already been calculated. If so, it gets the inverted matrix from the cache and skips the computation.
## Otherwise, it calculates the inverted matrix and sets the value of the inverted matrix in the cache via the setInvX function.

cacheSolve <- function(x, ...) {
        inv <- x$getInvX() 
        if(!is.null(inv)) {#Check if the inverted matrix is stored in cache
                message("getting cached data")
                return(inv)#return the stored inverse
        }
        data <- x$get()
        inv <- solve(data) #compute the inverse of the matrix
        x$setInvX(inv)
        inv
}