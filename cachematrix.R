## Matrix inversion is usually a costly computation
## and their may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly.
## This module implements functions for creating and
## caching matrixes.


## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	get <- function() x
	set <- function(data) {
		x <<- data
		i <<- NULL
	}

	get_inverse <- function() i
	set_inverse <- function(inverse) {
		i <<- inverse
	}

	list(
		set = set,
		get = get,
		set_inverse = set_inverse,
		get_inverse = get_inverse)
}


## Computes the inverse of the special "matrix".
## If the inverse has already been calculated
## (and the matrix has not changed)
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
	i <- x$get_inverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	data <- x$get()
	i <- solve(data, ...)
	x$set_inverse(i)
	i
}

