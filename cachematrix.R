## These functons will cache of the inverse of a matrix in order to reduce
## time spent on time consuming operations

## This function:
## Sets the value of the matrix
## Gets the value of the matrix
## Sets the value of the inverse
## Gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function checks if the inverse has already been calculated. It skips the 
## calculation of the inverse in case it is already done

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
