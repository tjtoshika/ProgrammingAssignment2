## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function returning a list containing function to set and get the value of matrix and set and get the value of inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## Write a short comment describing this function
## cacheSolve checks if the inverse of matrix is already in cache, if not it gets the matrix and cache the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message('getting cached inverse matrix')
                return(m)
        }
        matrix1 <- x$get()
        m <- solve(matrix1, ...)
        x$setinverse(m)
        m
}

