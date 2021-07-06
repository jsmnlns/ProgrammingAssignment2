#makeCacheMatrix is where to name the function
#next input x to represent in the matrix
#then identify needed list of functions: set, get, setInverse, getInverse as to get to matrix x

makeCacheMatrix <- function(x = matrix()) {
  
  #initializes the inverse as a NULL
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  #the functions to be stored into matrix x
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function(){inv}
  
  #function to obtain the inverse to matrix x
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

#cached data as where it computes inverse of the input in a matrix

cacheSolve <- function(x, ...)
  
#obtains the cache data
{
  inv <- x$getInverse()
  if(!is.null(inv)){
    
    #checks if the variable inv does not return a null, and if not it prints a message
    message("getting cached data!")
    #returns its inverse value
    return(inv) 
  }
  #calculates the obtained inverse value
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  #then returns a matrix of which the inverse of the "x"
  inv
}

