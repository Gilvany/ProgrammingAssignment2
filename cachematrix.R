## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #empty Variable that will represent the Cache for the inverse matrix
  set <- function(y) { 
    x <<- y #replace x matrix with new y matrix, for which set function is written
    m <<- NULL #it resets the cache (m) content within the function. 
  }
  get <- function() x #returns the original matrix
  setinverse <- function(inverse) m <<- inverse #caches the inversed matrix
  getinverse <- function() m #retunrs the inverse of m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) #returns all functions as defined
  }

## Write a short comment describing this function
#This function computes the inverse of makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() #get the inversed matrix that was cahched
  if(!is.null(m)) { #control
    message("getting cached data") #if true <- next function will run
    return(m) # if false 
  }
  data <- x$get() #get original matrix
  m <- solve(data, ...) #inverse the matrix into m
  x$setinverse(m) 
  m
}
