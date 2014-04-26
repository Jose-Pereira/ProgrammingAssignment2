## So this pair of functions help us
## save time when computing the inverse of a matrix
## first we find the inverse of a matrix and then 
## we cache this value so if the same matrix is used 
## again in the function, the inverse will be available
## available for display

## Overall this function creates a sprecial matrix 
## object that can cache it's inverse
## The final result is a list of 4 little "functions", get, set, setinverse, getinverse 

makeCacheMatrix <- function(x = matrix()) {  
  m <- NULL             
  set <- function(y) {  ## This function sets the value of the matrix
    m <<- NULL
  }
  get <- function() x   ## this function returns the value of the matrix
  setinverse <- function(inverse) m <<- inverse ## this function sets the value of the matrix that is going to be inversed
  getinverse <- function() m ##this function returns the value of the inverted matrix
  list(set = set, get = get, ##Here we state the list of functions created by cacheMatrix
       setinverse = setinverse,
       getinverse = getinverse)
}
##Now this is were the magic happens
##In cacheSolve is where we compute the value of the inverse
## but this is going to happen only in the case where we dont have
## the inverse in cache. 
cacheSolve <- function(x, ...) {
  m <- x$getinverse() ##In this line we call the inverse matrix
  if(!is.null(m)) {   ##If m is not empty (i.e we have the inverse value on cache) 
    message("getting cached data")
    return(m) ##return the cached inverse
  }                 ##(i.e the user is introducing a different matrix that the cached one)
  data <- x$get() ##we acces the new matrix
  m <- solve(data, ...) ##calculate the inverse
  x$setinverse(m) ##store the value of the inverse
  m ##and finally print the inverse :)
}
  