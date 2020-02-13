## Programming Assignment2 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## My function makeCacheMatrix is very similar to the example given in the assignment, with minor changes.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { 
    x <<- y 
    inv <<- NULL
  }
  get <- function() x 
  setsolv <- function(solve) inv <<- solve 
  getsolv <- function() inv 
  list(set = set, get = get,
       setsolv = setsolv,
       getsolv = getsolv)
}
## CacheSolve uses as argument the first function in order to work as expected.

cacheSolve <- function(x, ...) {
  inv <- x$getsolv()
  if(!is.null(inv)){
    message("Retrieving data...")
  }
  data <- x$get()
  inv <- solve(data)
  x$setsolv(inv)
  inv
 ## Return a matrix that is the inverse of 'x'
}

# HOE TO TESTE THIS r SCRIPT IN A EASY WAY

# First - create a vector: 
# > m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

# Second - Declare a variable assigning makeCacheMAtrix(-argument-):  
# > teste <- makeCacheMatrix(m1)

# Third - Call the function cacheSolve using the declared variable teste: 
# > cacheSolve(teste) 

# Fourth - The answer should be:

#[,1] [,2]
#[1,]    6    8
#[2,]    2    4

# fifth - Thank you very much for correcting my assignement... Remember to be mindfull and fair correcting it. I'll give my best
# to correct fairly my peer's assignments as well!
