## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	
	x_inverse <- NULL
	
	set <- function(a_matrix) {
		x         <<- a_matrix
		x_inverse <<- NULL
	}
	
	get <- function() {
		x_matrix
	}
	
	set_inverse <- function(inverse) {
		x_inverse <<- inverse
	}
	
	get_inverse <- function() {
		x_inverse
	}
	
	list(set        = set,
	     get        = get,
	     set_inverse = set_inverse,
	     get_inverse = get_inverse)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$get_inverse()
	
	if (is.null(inverse)) {
		message("calcuating the inverse of matrix")
		the_matrix <- x$get()
		inverse    <- solve(the_matrix, ...)
		x$set_inverse(inverse)
			
	} else {
		message("using cached inverse")
	}

	inverse	
}
