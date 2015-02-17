## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	
	my_inverse <- NULL
	
	set <- function(y) {
		x         <<- y
		x_inverse <<- NULL
	}
	
	get <- function() {
		x
	}
	
	setInverse <- function(inverse) {
		my_inverse <<- inverse
	}
	
	getInverse <- function() {
		my_inverse
	}
	
	list(set        = set,
	     get        = get,
	     setInverse = setInverse,
	     getInverse = getInverse)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	
	if (!is.null(inverse)) {
		message("getting cached inverse")
		return(inverse)
	}
	
	data    <- x$get()
	inverse <- solve(x, ...)
	x$setInverse(inverse)
	inverse
}

#cachemean <- function(x, ...) {
#	m <- x$getmean()
#	if(!is.null(m)) {
#		message("getting cached data")
#		return(m)
#	}
#	data <- x$get()
#	m <- mean(data, ...)
#	x$setmean(m)
#	m
#}
