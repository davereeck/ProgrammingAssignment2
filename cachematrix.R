## These functions calculate the inverse of a matrix, including checking to see if a
## cached copy of the result exists beforehand. Caching the entry prevents costly
## re-execution if the value already exists. 


## makeCacheMatrix creates a special matrix which caches it's inverse value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) #calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Check to see if the inverse has been calculated already, if so use that value.
## Otherwise, calculate the inverse of the parameter 'x'.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    print("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse()
  inv
}
