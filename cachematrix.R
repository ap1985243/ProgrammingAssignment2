## In order to save time and resources, this program
## commits the inverse of a matrix into the memory
## If that computation needs to be repeated, the program
## first checks if it calculated it earlier and returns accordingly

## makeCacheMatrix creates a special type of matrix 
## with added structure allowing for the caching of its inverse
## this saves time in case of repeated computations

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Following function cacheSolve checks if the inverse has been cached 
## or else it carries on with the computation, returning the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
