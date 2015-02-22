## Two functions has been created to cache inverse of matrix. That's usefull when loop function are used 
## and matrices used for computing inverse matrix are large.

# The first function called 'makeCacheMatrix' creates a special matrix containing functions,
# which initiate matrix for second function and keep inverse matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## The second function check if inverse of matrix has been calculated. If not, the function returns
## inverse of matrix and set it to special vector created by first function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
