#makeCacheMatrix returns a list of functions.
#This function stores a "matrix" object and also caches its inverse value

makeCacheMatrix<-function(x=matrix()){
    invmatrix <- NULL
    set <- function(y)
    {
        x <<- y
        invmatrix <<- NULL
    }
    
    get<-function() x
    setInverseVal <- function(inverse) invmatrix <<- inverse
    getInverseVal <- function() invmatrix
    list(set = set,
         get = get,
         setInverseVal = setInverseVal,
         getInverseVal = getInverseVal)
    
}

## This function calculates the inverse of the special "matrix" created by 
## makeCacheMatrix in the above code. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmatrix <- x$getInverseVal()
    if (!is.null(invmatrix)) {
        message("getting cached data")
        return(invmatrix)
    }
    mat <- x$get()
    invmatrix <- solve(mat, ...)
    x$setInverseVal(invmatrix)
    invmatrix
}
