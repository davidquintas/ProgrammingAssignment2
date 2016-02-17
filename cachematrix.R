## The following functions are created to reduce the computational cost

## makeCacheMatrix takes a matrix x as input (an empty matrix, by default)
## and gives a list of four elements (all functions)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve 
        #solve(X) calculates the inverse of the matrix X
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes x as an input. If x has already stored 
## the inverse matrix, cacheSolve prints "getting cached data"
## otherwise, it calculates the inverse (by means of solve function)

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        else{
                data <- x$get()
                inv <- solve(data, ...)
                x$setinv(inv)
                inv
        }
}