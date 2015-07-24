## Function makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse. Function cacheSolve computes the inverse of the special "matrix" 
## Any invertible square matrix first need to be passed into makeCacheMatrix 
## functio to make it as a special matrix. The saved output of makeCacheMatrix 
## should be passed into cacheSolve to create the inverse
## e.g. 
## > sample <- matrix(c(2, 2, 3, 2), 2, 2)
## > a <- makeCacheMatrix(sample)
## > cacheSolve(a)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


