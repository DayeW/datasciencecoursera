## Assignment: Caching the Inverse of a Matrix

## My functions create a special object "makeCacheMatrix" that 1) stores a matrix and caches its inverse, and 2) calculates or retrieves the inverse of the matrix, under "cacheSolve."
 

## Caching the inverse
## The following function creates a special "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
		
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)

}


## Computing the inverse
## The function below computes the inverse of the special "matrix" created by makeCacheMatrix. If the inverse is already calculated (and matrix unchanged), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getInverse()
		if(!is.null(i)) {
			message("getting cached data")
			return(i)
		}
		data <- x$get()
		i <- solve(data)
		x$setInverse(i)
		i
}
