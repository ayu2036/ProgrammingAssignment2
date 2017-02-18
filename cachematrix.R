## makeCacheMatrix - "Create an objects" that store its input matrix as well as 
## caching the inverse matrix
## This object contains the following method
## get - return the stored matrix
## set - store the matrix
## setinverse - store the inverse
## getinverse - retrieve the stored inverse or null if it doesn't exist

makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }
  get <- function() matrix
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve - Find out the inverse of the matrix
## it checks whether the input matrix has an inverse calculated or not
## if true, return it
## otherwise, retrieve the matrix, solve it and store it back

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
