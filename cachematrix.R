## The below functions makeCacheMatrix and cacheSolve are written to create 
## a special matrix object and cache its inverse. The other function is used to
## retrive the inverse from cache if already present.

## This function is used to create a special matrix object and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverseOfMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseOfMatrix <<- NULL
  }
  
  get <- function() x
  # Using solve function calculate the inverse the non singular matrix
  setInverseMatrix <- function(solve) inverseOfMatrix <<- solve
  
  getInverseMatrix <- function() inverseOfMatrix
  
  list( set = set, get = get, setInverseMatrix = setInverseMatrix, 
        getInverseMatrix = getInverseMatrix)

}


## If the inverse of the matrix provided is availabe in the cache, this function
## returns the inverse of matrix from the cache. Else it will calculate the
## inverse and store it in cache.

cacheSolve <- function(x, ...) {
  inverseOfMatrix <- x$getInverseMatrix()
  if(!is.null(inverseOfMatrix)) {
    message("Getting the inverse of matrix from cache.")
    return(inverseOfMatrix)
  }
  ## If the inverse is not found in cache, then calculate the inverse
  data <- x$get()
  inverseOfMatrix <- solve(data, ...)
  x$setInverseMatrix(inverseOfMatrix)
  inverseOfMatrix
}
