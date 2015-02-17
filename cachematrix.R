# The makeCacheMatrix function creates a special "matrix" object that 
# can cache its inverse.
# This object contain a list of functions that will set the matrix, 
# get the matrix, set the inverse of the matrix and get the inverse 
# of the matrix.

# The cacheSolve function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix. If the inverse has 
# already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


#############################################################
# This function takes an invertible matrix as an argument.
#
# When called it clears inverse cache and stores the supplied
# matrix.
#
# The set function populates the matrix storage x and clears
# the inverse cache so that any changes to the stored matrix
# will be detected and false results are avoided.
#
# The get function returns the stored matrix.
#
# The setinverse function stores the inverse (solved) matrix.
#
# The getinverse function returns the inverse (solved) matrix.
#############################################################

makeCacheMatrix <- function(x = matrix()) {
        # Clears the cache storage
        inv <- NULL
        # Stores the invertible matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Gets the invertible matrix
        get <- function() x
        # Stores the inverse (solved) matrix 
        setinverse <- function(inverse) inv <<- inverse
        # Gets the inverse (solved) matrix 
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#############################################################
# This function takes a special matrix object 'x' created by 
# makeCacheMatrix as an argument.
#
# It calls a function available in the special matrix object
# to retieve the cached inverse matrix stored in the special
# matrix object.
# The function tests if the retrieved matrix has been solved.
# If the matrix has been solved, a matrix that is the inverse 
# of 'x' is returned.
# Otherwise the function gets the invertible matrix stored in
# 'x', inverts(solves) the invertible matrix, and stores the 
# cache in the 'x' object. 
# The fuction then returns a matrix that is the inverse of 'x'
#############################################################
cacheSolve <- function(x, ...) {
        # Gets the cached matrix
        inv <- x$getinverse()
        # Tests if the cached matrix has data
        if(!is.null(inv)) {
                message("getting cached data")
                # returns the cached matrix and exits 
                # the function
                return(inv)
        }
        # Gets the matrix from 'x'
        data <- x$get()
        # Inverts (solves) the matrix 
        inv <- solve(data, ...)
        # Stores the solved matrix in 'x'
        x$setinverse(inv)
        # Return a matrix that is the inverse of 'x'
        inv
}
