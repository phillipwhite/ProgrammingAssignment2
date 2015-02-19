## These functions provide cached access to 
##	the inverse of a square invertible matrix

## Constructs a CacheMatrix object which caches a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	# Constructs and returns a special 'CacheMatrix object' 
	# which encapsulates (contains) both the matrix and its inverse
	#
	# Arguments:
	#	x: an invertible square matrix
	#
	# Returns:
	#	A list of the accessor methods (get and set) 
	#	for the matrix and its inverse
	
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



## Returns the matrix inverse of a CacheMatrix object

cacheSolve <- function(cache_matrix, ...) {
        # Calculates or obtains the cached inverse of a CacheMatrix object
	#
	# Arguments:
	#	cache_matrix: The 'CacheMatrix object' whose inverse is to be calculated
	#	   This must be created by the makeCacheMatrix function
	#
	#	...: other arguments for calculating the matrix inverse
	#
	# Returns:
	#	The  matrix inverse of the 'CacheMatrix object	
	
	inverse <- cache_matrix$getInverse()
	
	if ( is.null(inverse) ) {		
		message("cacheSolve(): calculating the inverse of the matrix")
		the_matrix <- cache_matrix$get()
		inverse    <- solve(the_matrix, ...)
		cache_matrix$setInverse(inverse)	
	} else {
		message("cacheSolve(): using the cached inverse of the matrix")
	}

	inverse	
}



## Tests the makeCacheMatrix and cacheSolve functions on a single matrix

testMatrix <- function(x) {
	# Runs a series of test on single matrix 
	#	of the makeCacheMatrix and cacheSolve functions
	# 	and prints results for each test
	#
	# Arguments: 
	#	x: a square invertible matrix
	#
	# Returns:
	#	TRUE if all tests pass, otherwise returns FALSE
	
# 	print("Testing with new matrix")
# 	print(x)
	
	cache_matrix <- makeCacheMatrix(x)
	test1 <- identical(cache_matrix$get(), x)
	test2 <- identical(cache_matrix$getInverse(), NULL)
# 	print("Completed tests 1, 2")
	
	cache_matrix$set(x)
	test3 <- identical(cache_matrix$get(), x)
	test4 <- identical(cache_matrix$getInverse(), NULL)
# 	print("Completed tests 3, 4")
	
	cache_matrix$setInverse(x)
	test5 <- identical(cache_matrix$getInverse(), x)
	test6 <- identical(cacheSolve(cache_matrix), x)
# 	print("Completed tests 5, 6")
	
	cache_matrix$setInverse(NULL)
	test7 <- identical(cache_matrix$getInverse(), NULL)
	test8 <- identical(cacheSolve(cache_matrix), solve(x))
# 	print("Completed tests 7, 8")
	
	print(c(test1, test2, test3, test4,
		test5, test6, test7, test8))
	
# 	print(solve(x))
# 	print(cache_matrix$getInverse())
# 	print("Completed test 5")

	test1 & test2 & test3 & test4 & test5 & test6 & test7 & test8	
}



## Tests the makeCacheMatrix and cacheSolve functions on several matrices single matrix

testRunner <- function() {
	# Runs a series of test on different matrices and
	# 	prints test results for each matrix
	#
	# Returns:
	#	TRUE if all tests pass, otherwise returns FALSE

	x <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
	test_matrix1 <- testMatrix(x)
	
	x <- matrix(1:4, nrow = 2, ncol = 2)
	test_matrix2 <- testMatrix(x)
	
	x <- matrix(runif(4), nrow = 2, ncol = 2)
	test_matrix3 <- testMatrix(x)
	
	x <- matrix(runif(100), nrow = 10, ncol = 10)
	test_matrix4 <- testMatrix(x)
	
	print(c(test_matrix1, test_matrix2, test_matrix3, test_matrix4))

	test_matrix1 & test_matrix2 & test_matrix3 & test_matrix4
}