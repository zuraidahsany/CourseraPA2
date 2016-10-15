## PROGRAMMING ASSIGNMENT 2: LEXICAL SCOPING

## Function #1: Creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) 
	{
  		inv <- NULL
		set <- function(y) 
			{
    				x <<- y
				inv <<- NULL
  			}
  		get <- function() x
  		setInverse <- function() inv <<- solve(x) 
  		getInverse <- function() inv
  		list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	}

## function#2: Computes the inverse of hte special "matrix" returned by the Function #1 above.
cacheSolve <- function(x, ...) 
	{
        	inv <- x$getInverse()
    		if (!is.null(inv)) 
			{
        			message("Cache-ing data")
        			return(inv)
    			}
    		mat <- x$get()
    		inv <- solve(mat, ...)
    		x$setInverse(inv)
    		inv
	}