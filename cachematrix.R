## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL # this is where the result of inversion is stored
  # A setter function, use this to set a matrix to object created by makeCacheMatrix function
  set <- function(y) {
    x <<- y
    m <<- NULL # it also initialises m to null
   } 
   get <- function() x # return the input matrix
  setInv <- function(inv) m <<- inv # set the inversed matrix
  getInv <- function() m # return the inversed matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getInv() # Return a matrix that is the inverse of 'x'
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get the matrix object
  m <- solve(data) # we solve it
  x$setInv(m) # we then set it to the object
  m # return the solved result       
}
