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
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	
	if (is.null(inverse)) {
		message("calcuating the inverse of matrix")
		the_matrix <- x$get()
		inverse    <- solve(the_matrix, ...)
		x$setInverse(inverse)
			
	} else {
		message("using cached inverse")
	}

	inverse	
}
