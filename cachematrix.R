## The following functions cache the inverse of a matrix and then recall that value to save on computational time and
## resources.

## Write a short comment describing this function
## The following is a function that creates a special caching matrix.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinv <- function(inv) m<<- inv
        getinv <- function() m
        list(
                set = set,
                get = get,
                setinv = setinv,
                getinv = getinv
        )

}


## Write a short comment describing this function
## This function calculates the inverse of the special matrix created above 
## but it searches to see if the result was already calculate

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)){
           message("Getting cached data...")
           return(m)
        }
        data <- x$get()
        m <- solve(m, ...)
        x$setinv(m)
        m
}
