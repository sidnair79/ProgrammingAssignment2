#----------------------------------------------------------------------------#

#Matrix inversion maybe a costly computation and there may be benefit in 
#caching the inverse of a matrix rather than computing it repeatedly. The 
#two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
        
#----------------------------------------------------------------------------#

#The cacheSolve function returns the inverse of the matrix. It checks if the 
#inverse has already been? If yes, then it gets the result and skips the 
#computation. If no, then it computes the inverse and sets the value in the 
#cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

#----------------------------------------------------------------------------#