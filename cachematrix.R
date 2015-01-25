## Use <<-  to assign a value to an object in an environment that is different 
## from the current environment
## Example usage:
## > X <- matrix(c(2,0,0,2),2,2)
## > t<-makeCacheMatrix(X)
## > cacheSolve(t)
##      [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5
## > cacheSolve(t)
## getting cached data
##     [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5


## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
				x <<- y
				inv <<- NULL		
		}
		get <- function() x
		setsolve <- function(solve) inv <<- solve
		getsolve <- function() inv
		list(set = set, get = get,
				setsolve = setsolve,
				getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)) {
        		message("getting cached data")
        		return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
