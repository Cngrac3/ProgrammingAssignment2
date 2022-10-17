## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function, makeCacheMatrix, creates a special "matrix" ,which is really a list containing a function to:
        ## 1. set the value of the matrix
        ## 2. get the value of the matrix
        ## 3. set the value of the inverse matrix
        ## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                               ## initialize m as NULL, holding value of matrix inverse 
    set <- function(y) {                     
        x <<- y                             
        m <<- NULL                         
    }
    get <- function() x        ## get the value of the matrix
    
    setinverse <- function(inverse) m <<- inverse  ## set the value of the inverse m
    getinverse <- function() m                     ## get the value of the inverse m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   
}

## Write a short comment describing this function

## This function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the mean in the cache via the setinverse function.

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
