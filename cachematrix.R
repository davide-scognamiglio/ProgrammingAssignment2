makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() {
        i
    }
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
#Now let's test the functions

#Here I create a matrix M and create its inverse, then I assign it to inverted_M
M <- matrix(c(1,2,4,5),2,2)
M1 <- makeCacheMatrix(M)
inverted_M <- cacheSolve(M1)

#Now let's check if we get identity matrix by performing matrix product between the matrix and its inverse
identity <- B %*% inverted_M

identity
