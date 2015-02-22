# The following two functions, involving matrix inversion, are designed to 
# work in tandom. Because matrix inversion can be computationally
# time-consuming, the first of the two functions, makeCacheMatrix(), takes as 
# its argument a matrix, creates (and returns) a special "matrix" object (viz., 
# a list object whose elements are themselves functions that manipulate the 
# matrix), and caches the inverted matrix. The second of the two functions, 
# CacheSolve(), computes the inverse of the special "matrix" returned by 
# makeCacheMatrix(). Specifically, if the the matrix has not changed and its 
# inverse has already been calculated, then CacheSolve() simply retrieves the 
# the previously-cached inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
    # Calculates the number of complete observations for each of the 
    # user-specified CSV files (monitors) and returns the results in the form 
    # of a summary table. An observation is deemed to be complete if both
    # the sulfate and nitrate values are not NA. Sulfate values are assumed to
    # be in Column 2, nitrate values in Column 3.
    #
    # Args:
    #   x: a matrix object to be inverted.
    #
    # Returns:
    #   A list object whose elements are functions designed to manipulate
    #   the "matrix" object.
    #
    # Usage:
    #   aMatrix = matrix(4, 3, 3, 2, nrow = 2, ncol = 2)
    #   anotherMatrix = matrix(1, 3, 2, 4, nrow = 2, ncol = 2)
    #   x <- makeCacheMatrix(aMatrix)
    #   x$getMatrix() -- get the value of the x matrix
    #   x$setMatrix(anotherMatrix) -- change the value of the matrix
    #   x$getInverve() -- get the cached inverted matrix
    #   x$setInverse(anotherMatrix) -- set (change) the cached value of the 
    #                                  inverted matrix
                
    # initialize the value of the inverted matrix
    invertedMatrix <- NULL
    
    # function used to set an object returned by makeCacheMatrix() with
    # values given by the matrix argument, y
    setMatrix <- function(y) {
        x <<- y
        invertedMatrix <<- NULL
    }
    
    # function used to return (i.e., get) the value of the to-be-inverted
    # matrix, x
    getMatrix <- function() x
    
    # function assigns (sets) the value of the cached inverted matrix with 
    # matrix argument, inverted
    setInverse <- function(inverted) invertedMatrix <<- inverted
    
    # function used to return (get) the value of the cached inverted matrix
    getInverse <- function() invertedMatrix
    
    # return value
    list(setMatrix = set,
         getMatrix = get,
         setInverse = setInverse,
         getInverse = getInverse)
    }
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
