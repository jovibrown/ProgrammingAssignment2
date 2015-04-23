## This exercise presents a pair of functions that, when used together, 
#cache a matrix's inverse and retrieve the cached information. The functions 
#are makeCacheMatrix and cacheSolve, and they are used with the assumption 
#that the matrix supplied is always invertible.

#MakeCacheMatrix does the following:
#1. Sets the matrix's value.
#2. Gets the matrix's value.
#3. Sets the matrix's inverse value.
#4. Gets the matrix's inverse value.

#CacheSolve calculates the matrix's inverse as was created with the 
#makeCacheMatrix function. To do this, cacheSolve takes the following steps:
#1. Checks to see if the inverse has already been calculated.
#2. If yes, then cacheSolve retrieves the matrix's inverse from the cache 
#(and thus does not compute inverse).
#3. If no, then cacheSolve computes the matrix's inverse; and, via the 
#setinverse function, sets the matrix's inverse value.



##Now, let's get started and look at the functions.


#The makeCacheMatrix makes a cache of matrix 'x'.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                 I<<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The cacheSolve function returns a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- inverse(data, ...)
        x$setinverse(I)
        I
}


##Hope you enjoyed this exercise.
