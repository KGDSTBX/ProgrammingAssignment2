## The function(s) creates an inverse of a matrix ( assumed to be invertible)
## To esure optimum use if resources this value is then cached

## This function creates matrix and can give information including inverse of the matrix

makeCacheMatrix<- function(x = matrix(...) ){
    ## initializing the inv(matrix) to be cached
    inv <- NULL
    ## setting the value of the matrix
    set <- function(y) {
        x <<- matrix(y,nrow=2,ncol=2)
        inv <<- NULL
    }
    ## getting the value of the matrix
    get <- function() x
    ## setting the inverse of the matrix 
    setinverse <- function(solve) inv <<- solve
    ## getting the inverse of the matrix 
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function calculates inverse of a matrix if not available in cache or return value from cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    ## checking if inverse is already calculated
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if Inverse not already calculated then calculating inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
