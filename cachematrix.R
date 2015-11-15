## These functions create a special matrix and calculate the inverse.
## Once the inverse is calculated is stored with the matrix.
## If the inverse is called a second time the cached inverse is called instead 
## of recalculating the inverse.

## makeCacheMatrix creates a special 'matrix', which is a list of functions.
## The functions are: set the value of the matrix, get the value of the matrix,
## set the value of the inverse of the matrix, get the value of the inverse
## of the matrix.
## For example to get the inverse use: matrix$get(), where the object was assigned to matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix if it has not already been
## calculated, the invest is then stored with the matrix. 
## If it has already been calculate it just returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
