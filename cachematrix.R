## The functions below allow the user to cache the inverse of a matrix so that
## it can be recalled rather than being calculated each time it is needed.


## makeCacheMatrix creates an object containing a matrix as well as functions allowing the matrix to cache its inverse.
## It takes a matrix as its argument, and returns a cacheable matrix object.

makeCacheMatrix <- function(mat = matrix()) {
	inverse <- NULL
	set <- function(newMatrix) {
		mat <<- newMatrix
		inverse <<- NULL
	}
	get <- function() mat
	setInverse <- function(newInverse) inverse <<- newInverse
	getInverse <- function() inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes a cacheable matrix as its argument, and checks whether the matrix's inverse has been cached.
## If so, it returns it. If not, it calculates the inverse, caches it for later use, and returns it.

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- mat$getInverse()
        if (!is.null(inverse)) {
        	message("getting cached data")
        	return(inverse)
        }
        data <- mat$get()
        inverse <- solve(data, ...)
        mat$setInverse(inverse)
        return(inverse)
}
