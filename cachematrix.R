#function to creates a special matrix object that can cahce its inverse


#makeCacheMatrix creates four functions 
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse
# 4. get the inverse 

makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y    
    m <<- NULL                                 #store matrix in cache
  }
  get <- function() x
  setInverse <- function(inverse) m <<-inverse #set inverse matrix
  getInverse <- function() m                   #get inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)                #create list of functions
  
}

#functon to computes the inverse of the special matrix created by makeCacheMatrix 
#and calculates the inverse matrix 
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if ( ! is.null(m)) {                         #check if the inverse has been calculated 
    print("getting cached data")               #print the message that indicate is just cache
    return(m)                                  # return cache
  }
  m <- solve(x$get())                          #calculate the inverse matrix
  x$setInverse(m)                              #store the inverse matrix in cache
  m
}
