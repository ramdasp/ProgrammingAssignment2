## This programming assignment required us to write an R
## function that is able to inverse a matrix and cache the results. 
## If the contents of the matrix are constant, it may make
## more sense to cache the value of its inverse value so that when we need it again, it
## can be looked up in the cache rather than recomputed. 

## Function makeCacheMatrix creates a special "vector", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m   <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get        <- function()    {x}
        setinverse <- function(inv) {m <<- inv}
        getinverse <- function()    {m}
        list(set        = set
		   , get        = get
		   , setinverse = setinverse
		   , getinverse = getinverse)
}

## Function cacheSolve returns the inverse matrix using the variable created with the above function.
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the value to the cache variable 
## via the setinverse function.
cacheSolve <- function(x=matrix(), ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            ## 'message()' prints to the console
		    message("getting cached data")
        }
		else {
			data <- x$get()
			m <- solve(data, ...)
			x$setinverse(m)
		}
        ## Return a matrix that is the inverse of 'x'
		m
}
