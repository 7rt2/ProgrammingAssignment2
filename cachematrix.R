## Put comments here that give an overall description of what your
## functions do

##these two functions will take a matrix and calculate its inverse
##if the matrix is invertible

## Write a short comment describing this function

##this first function creates a matrix object and allows us to cache the inverse
##it contains 4 functions (set, get, setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x<<-y
		m<<-NULL
	}
	get<-function()x
	setinverse<-function(solve) m<<-solve
	getinverse<-function() m
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## the second function checks if the inverse of the matrix is stored in the cache
## if it is, it skips the calculation and returns the inverse
## if not, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	##check for inverse in cache
	m <- x%getinverse()
	if(!is.null(m))  {
		message("getting cached data")
		return(m)
	}
	##calculate inverse if not in cache
	data <- x$get()
	m <- solve(data, ...)
	m$setinverse(m)
	m
}
