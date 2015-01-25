## makeCacheMatrix will create a special "Matrix" object
## that will allow for cache of its inverse.

## Describing this function: cache the matrix inversion


makeCacheMatrix <- function(x = matrix()) {
z <- NULL
set <- function(y){
	x <<- y
	z <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) z <<- solve
	getmatrix <- function() z
	list(set=set, get=get,setmatrix=setmatrix, getmatrix=getmatrix)
}



## cacheSolve will compute the inverse of the special matrix
## that is returned by makeCacheMatrix function
## if already calculated the function will return the inverse
## from the cache.

cacheSolve <- function(x=matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getmatrix()
        if(!is.null(z)){
        	message("getting cached data")
        	return(z)
        	}
        	matrix <- x$get()
        	z <- solve(matrix, ...)
        	x$setmatrix(z)
        	z
}
