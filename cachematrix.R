## makeCacheMatrix creates a special matrix object, and cacheSolve either calculates its inverse or get its inverse from the cache and return it if it has already been calculated.

## makeCacheMatrix can set the matrix and its inverse, store them and pass them to other variables. It returns a list of all these functions inside it.

makeCacheMatrix <- function(x = matrix()) {
                invmat <- NULL
                set <- function(y) {
                                x <<- y
                                invmat <- NULL
                }
                
                get <- function() {
                                x
                }
                setinverse <- function(inv) {
                				invmat <<- inv
                }
                getinverse <- function() {
                                invmat
                }
                list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve checks whether the inverse of the given matrix has already been calculated. If so, then it will look for the inverse from the cache and return it; and if not, then it will calculate it and return it.

cacheSolve <- function(x, ...) {
                invmat <- x$getinverse()
                if(!is.null(invmat)) {
                                message("getting cached data")
                }
                else {
                                data <- x$get()
                                invmat <- solve(data, ...)
                                x$setinverse(invmat)
                }
                invmat
}
