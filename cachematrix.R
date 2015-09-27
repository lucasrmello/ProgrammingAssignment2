## Hello, here is my code, the first function will store a matrix in te global enviroment and the second one is supposed to
## get you the inverse of this matrix using the solve() function
## in the end of the script there is already an example of check for the sake of saving you some time when evaluating the code

## Here is the function used for storing the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) { ## changes the function stored in the main function
    x <<- y
    inv <<- NULL
  }
  get <- function() x ##returns the matrix x stored in the main function
  setinverse <- function(inverse) inv <<- inverse ## set inverse and getinverse just store de value of inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## list with the four functions that make the main function
}

##this function, Cachesolve will retrieve you the inverse of the matrix inputed in makeCachematrix 

  cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data.")
      return(inv) ##  until this part, cacheSolve verify the value of inverse and if its already exist in memory 
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv ## data gets the matrix stored with makeCache matrix and inv use the function solve to calculate the inverse of the matrix. setinverse stores it and inv will show it in r 
  }
  
 ## example,  
  x = rbind(c(1, -1/4), c(-1/4, 1)) ## creates an array
  example = makeCacheMatrix(x) ## stores the array in memory
  example$get() ## use it to confirm that your array is stored in memory
  
  cacheSolve(example) ## will make an invertion of your array, you can use the previous line to confirm that the new array was stored
