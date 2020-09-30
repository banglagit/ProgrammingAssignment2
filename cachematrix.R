## it accelerates the inverse matrix calculation
## sometimes it is cumbersome on the part of cpu to actually carry out computing
## out computing task in case of big data. so in those cases,
## it is useful to take advantage of cache memory which is very fast 
## component to work with. This function no matter how cryptic it looks 
## does exactly the same.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



## the workflow is like: it first checks if there is a already calculated 
## inv_function, If there is one thw progran uses that but if there is not one
## the program rerruns the function and computes it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
