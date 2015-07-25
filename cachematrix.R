makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # sets the value of m to NULL
  setmatrix <- function(y){ #set the value of the matrix
  x <<- y  # caches the inputted matrix so that cacheSolve can check whether it has changed
  m <<- NULL  # sets the value of m to NULL
}
getmatrix <- function() x
setmatrix <- function(solve) m<<- solve #set the inverse of the matrix
getmatrix <- function() m # get the value of the matrix
list(setmatrix=setmatrix, getmatrix=getmatrix, # creates a list to house the four functions
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
	# Need to compare matrix to what was there before 
	 m <- x$getmatrix()  # if an inverse has already been calculated this gets it
	if(!is.null(m)){  # check to see if cacheSolve has been run before
      message("getting cached data")
      return(m) # return the matrix
    }
    matrix <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
    m <- solve(matrix, ...) # compute the value of the inverse of the input matrix
    x$setmatrix(m) # run the setmatrix function on the inverse to cache the inverse
    m  # return the inverse
}
