## This is R Programming Assignment 2. In this, we would create a function which will return the inverse
## of a matrix. The inverse of matrix will be computed only if it is not already available in cache.
## This function creates a special matrix that can cache it's inverse!
makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(invm) inv <<- invm
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()
    if(!is.null(inv)){
      print("Data returned from cache")
      return(inv)
    }
    d<- x$get()
    inv<- solve(d)
    x$setinv(inv)
    inv
}