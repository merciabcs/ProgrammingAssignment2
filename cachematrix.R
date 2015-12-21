## 2 functions to compute inverse matrix or load it from cache if it 
## already computed.

## Function 1:
## Creates a list containing functions to set matrix, get matrix, 
## set inverse matrix and get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setinv <- function(solve) a <<- solve
    getinv <- function() a
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## function 2 
## solve the inverse of a matrix
## it also checks if the inverse of the matrix is already loaded into 
## cache and uses the cache before calculating it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    a <- x$getinv()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    data <- x$get()
    a <- solve(data, ...)
    x$setinv(a)
    a
}
