## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# Constructs and returns a special 'CacheMatrix object' 
	# which encapsulates (contains) both the matrix and its inverse
	#
	# Arguments:
	#	x: a square invertible matrix
	#
	# Returns:
	#	A list of the accessor methods (get and set) for the matrix and its inverse
	#
	# Usage: 
	#	Must call the cacheSolve function to cache the inverse of the matrix
	#	before calling the getInverse() method	
	
	x_inverse <- NULL
	
	set <- function(a_matrix) {
		x         <<- a_matrix
		x_inverse <<- NULL
	}
	
	get <- function() {
		x
	}
	
	setInverse <- function(inverse) {
		x_inverse <<- inverse
		# TODO insert call to cacheSolve here
		#      but avoid infinite recursion.
	}
	
	getInverse <- function() {
		x_inverse
	}
	
	list(set        = set,
	     get        = get,
	     setInverse = setInverse,
	     getInverse = getInverse)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # Returns a matrix that is the inverse of 'x'
	#
	# Arguments:
	#	x: The 'CacheMatrix object' whose inverse is to be calculated
	#	   This 'CacheMatrix object' must be created by the makeCacheMatrix function
	#	...: other arguments for calcualting the matrix inverse
	#
	# Returns:
	#	The inverse matrix of the 'CacheMatrix' object		
	inverse <- x$getInverse()
	
	if ( is.null(inverse) ) {
		message("cacheSolve: calculating the inverse of matrix")
		the_matrix <- x$get()
		inverse    <- solve(the_matrix, ...)
		x$setInverse(inverse)
			
	} else {
		message("cacheSolve: using cached inverse of matrix")
	}

	inverse	
}
