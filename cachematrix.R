##	These functions allow an invertible matrix be inverted and 
##	cached so when required more than once it does not have to 
##	be computed again


makeCacheMatrix <- function(x = matrix()) {
	##	Creates special matrix and can cache its inverse
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInvMatrix <- function(invMatrix) m <<- invMatrix
	getInvMatrix <- function() m
	list(set = set, get = get,
			setInvMatrix = setInvMatrix,
			getInvMatrix = getInvMatrix)
}

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	m <- x$getInvMatrix()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInvMatrix(m)
	m
}