makeCacheMatrix <- function(x = matrix()) {
	  #this function is similar to the makeVector example given
	  # the variable 'inv' (instead of m) is put to NULL
 	  inv <- NULL
	  #'set' will override x and put inv to NULL
        set <- function(y) {          
		    x <<- y
                inv <<- NULL
        }
	 #get will display the catched matrix
        get <- function() x
	 #setinverse will catch the inversed matrix
        setinverse <- function(inverse) inv <<- inverse
	#and getinverse is used to display the catched inversed matrix
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

cacheSolve <- function(x, ...) {
	  #first, it checks whether there is a catched value of inversed matrix
 	  inv <- x$getinverse()
	  #if there is a value, then display it and return it (stop)
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
	  #else, get the matrix, solve/inverse it, and catch that value
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}
