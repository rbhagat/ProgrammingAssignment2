## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function creates a special "matrix", which is a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list( set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)	
}


## Write a short comment describing this function
## cacheSolve function calculates the inverse of the special "matrix" created by the makeCacheMatrix function.
## However, it first checks whether the inverse matrix has already been calculated. If so, it get the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data adn sets the value
## of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inverse <- x$getinverse()
	if(!is.null(inverse)){
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data,...)
	x$setinverse(inverse)
	inverse
}
