## Name of the developer: Aurpita Roy
#Assignment02: Caching the Inverse of a Matrix

# The title of first funcion is makeCacheMatrix & the second function is cacheSolve.
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#Its functions are as follows:
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

# Small example to use the functions
mat = matrix(c(2:5), ncol =2)
VV = makeCacheMatrix(mat)
cacheSolve(VV)
cacheSolve(VV)

