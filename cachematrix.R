## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) { 
                # <<- operator is used to assign to a variable outside its 
                # local environment
                x <<- y
                s <<- NULL
        }
        get <- function() x
        
        # the matrix is inverted using the function stored in "setinverse"
        # this function is then stored in the "getinverse" 
        setinverse <- function(inverse) s <<- inverse
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # the below function will first determine if the matrix passed into "cacheSolve" has already been 
        # inverted using the "makeCachematrix" function above. If so, it will return the cached matrix.
        s <- x$getinverse ()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        # If the matrix has not been already cached, it will will invert the matrixe using 
        # "solve(data, ...)" and sets the inverse matrix in the cache with setmean function.
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
