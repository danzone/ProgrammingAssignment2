## These two functions optimize the calculation of the inverse of a matrix.
## The first makeCacheMatrix(m = matrix()) build an object that is
## capable to store a matrix and its inverse, the first time when
## it is calculated. If the content (the matrix) changes, the cached
## inverse will be deleted.
## The second function performs the calculation
##
## Usage:
## cm <- makeCacheMatrix(matrix(c(1,0,0,1),nrow=2,byrow=TRUE))
## cacheSolve(cm)
##

## Another example: calculate inverse and verify that
## the product is the identity matrix.
##
## cm <- makeCacheMatrix(diag(c(2,1),nrow=2,ncol=2))
## 
## cacheSolve(cm)
##     [,1] [,2]
##[1,]  0.5    0
##[2,]  0.0    1

## mc <- cacheSolve(cm)

## cm$get() %*% mc
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1

 
## This function build an object (represented with a list)
## to model a matrix which supports the caching of inverse
## calculation. The list is made by sub-functions that
## let to set the matrix, its inverse, and to get both, via methods
## set(), setinverse(), get(), getinverse().

makeCacheMatrix <- function(x = matrix()) {

	## Return a list which represents
	## a matrix with a cacheable inverse

	inv <- NULL

	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function() x

	setinverse <- function(inverse) inv <<- inverse

	getinverse <- function() inv


	list(set = set, get = get,
		setinverse = setinverse, getinverse = getinverse)
}


## This function takes a cache matrix as argument
## and return its inverse. It calculates the inverse only when necessary:
## the first time when it's being invoked. Successive invocations,
## if the matrix content isn't changed, will return the
## cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getinverse()

	if (!is.null(i)) {
		message("getting cached inverse")
		return (i)
	}

	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)

	i
}
