## The following functions facilitate the caching of inverse matrices


## This function creates a list of functions for storing
## and retrieving cached inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x<<-y
      m<<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(matrix) m <<- matrix
    getInverseMatrix <- function() m
    list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
    
}

## This function defines the logic in solving and returning cached inverse of a matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverseMatrix()
    if(!is.null(m)){
      message("getting inverse of matrix")
      return(m)
      
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverseMatrix(m)
    m

}
