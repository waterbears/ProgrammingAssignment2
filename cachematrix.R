## A matrix object that caches inverses

## returns an object representing the matrix with getter and setter
## for both the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Finds the inverse of a chached matrix object
## If inverse has been cached, the cached version
## is returned.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message('getting cached data')
        return(i)
    }
    x$setinv(solve(x$get()))
    x$getinv()
}
