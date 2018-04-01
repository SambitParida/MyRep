# makeCacheMatrix returns a list of functions.
# This function stores a "matrix" object and also caches its inverse value
#
# Example:-
# m <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)    
# > m
#       [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# m1 <- makeCacheMatrix(m)


makeCacheMatrix<-function(x=matrix()){
    invmatrix <- NULL # initialization of invmatrix with NULL
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
# cacheSolve(m1)
#
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5


cacheSolve <- function(x, ...) {
    invmatrix <- x$getInverseVal()
    if (!is.null(invmatrix)) {
        message("Cache Data Retrieved")
        return(invmatrix)
    }
    mat <- x$get()
    invmatrix <- solve(mat, ...)
    x$setInverseVal(invmatrix)
    invmatrix
}
