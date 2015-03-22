## This pair of functions extends a matrix definition to include
## a mechanism for caching the inverse 
## Usage: First call yourresult<-makeCacheMatrix(yourmatrix).
##        Then call yourmatrixinverse<-cacheSolve(yourmatrix) for the inverse
##        Repeated calls will use the cached copy of the inverse

## makeCacheMatrix extends a matrix
##     arguments: 
##         numeric matrix x
##     returns:
##         list object with data and accessor functions
##             object$set(y = matrix())
##             object$get(y = matrix())
##             object$setsolve(y = matrix()) - Use cacheSolve function instead
##             object$getsolve(y = matrix()) - Use cacheSolve function instead

makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solve)  s <<- solve
    
    getsolve <- function() s
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}



## cacheSolve returns the inverse of a matrix created with makeCacheMatrix
##     arguments: 
##         x - extended matrix object returned by "makeCacheMatrix"
##     returns:
##         inverse of the matrix stored in x.  Repeated call use a cached copy

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
    
    
}
