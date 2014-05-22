## Functions to get cached inverse value of matrix if the cached version exists 
## or to compute the inverse of a matrix if the cached value does not exist.


## this function is to compute the inverse of a matrix
makeCacheMatrix <- function(x = numeric()) {
	# this function is to compute the inverse of a matrix
	
	# if an object is called without a method
	m <- NULL
	
	# sub-function to set value to x
	set <- function(y) {
	x <<- y
	m <<- NULL
    }

	# sub-function to get and assign the input data 
	get <- function() x
	
	# sub-function to solve for the matrix inverse: they do same functionality, just different names
	setsolve <- function(solve) m <<- solve
	setinverse <- function(solve) m <<- solve
	
	# sub-function to get the cached result, if any.  Same functionality, just different names
	getsolve <- function() m
	getinverse <- function() m	
	
	# return a list of results
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve,
		setinverse = setinverse,
		getinverse = getinverse)
}


## function to get the inverse of a matrix, either by computing or getting the cache if possible
cacheSolve <- function(x, ...) {
	# function to get the inverse of a matrix, either by computing or getting the cache if possible
	# see if there is a cached result
	m <- x$getsolve()
	if(!is.null(m)) {
		# m has been cached before, just return the cached value
		message("getting cached data")
		return(m)
	}
	# no m has not been cached, so obtain x
	data <- x$get()
	# compute the inverse of x
	m <- solve(data, ...)
	# cache the result
	x$setsolve(m)
	# and return the result
	m
}







