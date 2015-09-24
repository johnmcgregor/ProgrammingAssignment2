##	These functions allow an invertible matrix be inverted and 
##	cached so when required more than once it does not have to 
##	be computed again.


makeCacheMatrix <- function(x = matrix()) {

	
	m <- NULL

	##	Assign the special matrix
	set <- function(y){
		x <<- y
		m <<- NULL
	}

	##	Return the special matrix
	get <- function() x

	##	Assign the inverse of the special matrix
	setInvMatrix <- function(invMatrix) m <<- invMatrix

	##	Return the inverse of the special matrix
	getInvMatrix <- function() m

	##	Store the lot
	list(set = set, get = get,
			setInvMatrix = setInvMatrix,
			getInvMatrix = getInvMatrix)
}

cacheSolve <- function(x, ...) {
	##	Return a matrix that is the inverse of 'x'
	##	If the matrix has already been inverted and 
	##	cached, return the cached value

	m <- x$getInvMatrix()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}

	##	Otherwise, invert the matrix, cache it and return it
	data <- x$get()
	m <- solve(data, ...)
	x$setInvMatrix(m)
	m
}