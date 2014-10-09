## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function


## creates a special matrix object that holds its inverse and expose methods
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #general get
  set <- function(value) {
    #value
    x <<- value
    inv <<- NULL
  }
  #general set
  get <- function() x
  #set inverse of data matrix
  set_inv <- function(ivval) { inv <<- ivval }
  #get inverse of data matrix  
  get_inv <- function() { inv }
  #just the list of methods
  list( set = set, get = get, set_inv = set_inv, get_inv = get_inv )
}


## Write a short comment describing this function
## implements the caching logic
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x$get()  
  inversMatrix <- x$get_inv()
  if(is.null(inversMatrix)){
    #we must set the inverse
    x$set_inv(solve(x$get()))
    
    #after the inverse set, return it
    print("setting the value of the inverse for the first time")
  }
  
  # if used for tests, code could be cleaner w/o it
  if(!is.null(inversMatrix)){
    print("the value is from cache")
    inversValue <- x$get_inv()
    inversValue
  }
}

#the test:
initialData <- matrix(c(1,2,-1,0), 2,2)

initialDataSpecial <- makeCacheMatrix(initialData)

cacheSolve(initialDataSpecial)
