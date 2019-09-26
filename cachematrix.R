## It might be ineficient to re-calculate in a function when a value has already being calculated. 
## The following functions uses inverse matrixes already calculated and saved as cached data trying 
## to make the process of obtaining the inverse of a matrix more agile.

## The function makeCacheMatrix creates an "special" matrix and creates a function that gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## The function cacheSolve check if the inverse of the "special" matrix has been calculated and cached.
## If it has been already calculated, it returns the cached inverse, if it has not been calculated, 
## it finds it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Example
j <- matrix(1:4, 2, 2)
k <- makeCacheMatrix(j)
cacheSolve(k)
