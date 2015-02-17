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

testMatrix <- function(x) {
	print("Testing with new matrix")
	#print(x)
	
	cache_matrix <- makeCacheMatrix(x)
	print(identical(cache_matrix$get(), x))
	print(identical(cache_matrix$getInverse(), NULL))
	print("Completed test 1")
	
	cache_matrix$set(x)
	print(identical(cache_matrix$get(), x))
	print(identical(cache_matrix$getInverse(), NULL))
	print("Completed test 2")
	
	cache_matrix$setInverse(x)
	print(identical(cache_matrix$getInverse(), x))
	print(identical(cacheSolve(cache_matrix), x))
	print("Completed test 3")
	
	cache_matrix$setInverse(NULL)
	print(identical(cache_matrix$getInverse(), NULL))
	print(identical(cacheSolve(cache_matrix), solve(x)))
	print("Completed test 4")
	
	#print(solve(x))
	#print(cache_matrix$getInverse())
	#print("Completed test 5")
	
}

testRunner <- function() {
	x <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
	testMatrix(x)
	
	x <- matrix(1:4, nrow = 2, ncol = 2)
	testMatrix(x)
	
	x <- matrix(runif(4), nrow = 2, ncol = 2)
	testMatrix(x)
	
	x <- matrix(runif(100), nrow = 10, ncol = 10)
	testMatrix(x)
}


