## These two functions will create the inverse of a matrix and store it, then when asked
## to invert the matrix will check and grab the inverse from the cache instead of computing
## from scratch again

## The first function inverts the matrix and stores into a cache

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)

  
}


## The second function checks to see if the inverse matrix is stored and returns the matrix from 
## the cache if found

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m

}
