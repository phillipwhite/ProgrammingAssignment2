## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# Constructs and returns a special 'CacheMatrix object' 
	# which encapsulates (contains) both the matrix and its inverse
	#
	# Arguments:
	#	x: an invertible square matrix
	#
	# Returns:
	#	A list of the accessor methods (get and set) for the matrix and its inverse
	
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
	#	...: other arguments for calculating the matrix inverse
	#
	# Returns:
	#	The inverse matrix of the 'CacheMatrix' object	
	
	inverse <- x$getInverse()
	
	if ( is.null(inverse) ) {		
		message("cacheSolve(): calculating the inverse of the matrix")
		the_matrix <- x$get()
		inverse    <- solve(the_matrix, ...)
		x$setInverse(inverse)	
	} else {
		message("cacheSolve(): using the cached inverse of the matrix")
	}

	inverse	
}


testRunner <- function() {
	x <- matrix(1:4, nrow = 2, ncol = 2)
	textMatrix(x )
	
}

testMatrix <- function(x) {
	cache_matrix <- makeCacheMatrix(x)
	identical(cache_matrix$get(), x)
	identical(cache_matrix$getInverse(), NULL)
	
	cache_matrix$set(x)
	identical(cache_matrix$get(), x)
	identical(cache_matrix$getInverse(), NULL)
	
	cache_matrix$setInverse(x)
	identical(cache_matrix$getinverse(), x)
	identical(cacheSolve(x), x)
	
	cache_matrix$setInverse(NULL)
	identical(cache_matrix$getinverse(), NULL)
	identical(cache_matrix$getinverse(), solve(x))
}
