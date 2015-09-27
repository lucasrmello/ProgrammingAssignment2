## Hello, here is my code, the first function will store a matrix in te global enviroment and the second one is supposed to
## get you the inverse of this matrix using the solve() function
## in the end of the script there is already an example of check for the sake of saving you some time when evaluating the code

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
  cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data.")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  }
  
 ## example 
  x = rbind(c(1, -1/4), c(-1/4, 1))
  example = makeCacheMatrix(x)
  example$get()
  
  cacheSolve(example)
