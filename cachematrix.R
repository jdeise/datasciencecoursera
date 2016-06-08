## The following two functions create a matrix object, as well as the 
## function to compute the inverse of this object taking into account 
## whether the inverse has already been calculated, in which case the
## inverse will be retrived from the cache.

## This function creates a list containing a function that 1) sets the value
## of a matrix 2) get the value of matrix 3) set the value of the inverse of 
## the matrix and 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of the matrix, after first checking if 
## the inverse has already been computed. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
        		message("getting cached data.")
        		return(inv)
        }
		data <- x$get()
		inv <- solve(data)
		x$setinverse(inv)
		inv
	
}

