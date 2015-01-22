## Funcions makeCacheMatrix and cacheSolve are used together to 
## calculate and cache the inverse of a invertible matrix.


## A generator function which stores a cached inverse of the invertible matrix x

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    
    ## Sets free variable "x" (the matrix to be inverted) in parent environment
    ## to the argument y
    set <- function(y) {
      x <<- y
      inv_m <<- NULL
    }
    
    ## Returns the original matrix x
    get <- function() x
    
    ## Sets the cached inverse in parent environment to the argument inv
    setinv <- function(inv) inv_m <<- inv
    
    ## Returns the cached inverse matrix (may be NULL if not yet calculated)
    getinv <- function() inv_m
    
    ## Returns a list of functions defined above
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Determines the inverse of an invertible matrix. The
## argument "x" must have been created with makeCacheMatrix.
## The inverse is calculated during the first call to cacheSolve,
## subsequent calls retrieve the cached inverse matrix.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of x (actually of the original 
    ## matrix that is represented by x)
    inv_m <- x$getinv()
    if(!is.null(inv_m)) {
      message("getting cached data")
      return(inv_m)
    }
    ## Get the original matrix
    m <- x$get()
    ## Calculate the matrix inverse (assume invertible)
    inv_m <- solve(m)
    ## Cache calculated inverse for future use
    x$setinv(inv_m)
    inv_m
}
