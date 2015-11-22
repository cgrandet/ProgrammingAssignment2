
## This function takes a matrix and creates a special list to
## set and get the value of the matrix, and set and get the value 
## of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a matrix and returns its inverse.
## If it has been already calculated, it return the inverse 
## from the cache memory. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached matrix")
    return(i)
  }
  matrix_data <- x$get()
  i <- solve(matrix_data, ...)
  x$setinverse(i)
  i
}
