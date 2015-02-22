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
    # This function takes as its argument a matrix, creates (and returns) a 
    # special "matrix" object (viz., a list object whose elements are themselves 
    # functions that manipulate the matrix), and caches the inverted matrix.  
    # This function is designed to work in tandem with the cacheSolve() (see
    # usage notes). No error checking; the function assumes that the matrix is 
    # invertible.
    #
    # Args:
    #   x: a matrix object to be inverted.
    #
    # Returns:
    #   A list object whose elements are functions designed to manipulate
    #   the "matrix" object. If the list object returned is assigned to
    #   x, then the functions are accessed thusly:
    #   x$getMatrix() -- get the value of the x matrix
    #   x$setMatrix(anotherMatrix) -- change the value of the matrix
    #   x$getInverve() -- get the cached inverted matrix
    #   x$setInverse(anotherMatrix) -- set (change) the cached value of the 
    #                                  inverted matrix
    #
    # Usage:
    #   aMatrix = matrix(4, 3, 3, 2, nrow = 2, ncol = 2)
    #   cached <- makeCacheMatrix(aMatrix)
    #   .
    #   .
    #   .
    #   cached2 <- cacheSolve(cached)
                
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
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    # Returns the inverse of matrix given by the argument x. If available,
    # the function using a previously cached value for the inverted matrix.
    # More specifically, the fuction works in tandem with makeCacheMatrix().
    # See documentation for that function for additional details.
    #
    # Args:
    #   x: a matrix object to be inverted.
    #
    # Returns:
    #   The inverted matrix.
    #
    # Usage: see documentation for makeCacheMatrix().
    
    # has the matrix already been inverted (cached)? If so, return the 
    # cached value
    invertedMatrix <- x$getInverse()
    if(!is.null(invertedMatrix)) {
        message("Getting cached data...")
        return(invertedMatrix)
    }
    
    # otherwise, get the to-be-converted matrix via getMatrix(), invert it,
    # cache the inverted matrix via setInverse(), and return the inverted 
    # matrix
    matrixData <- x$getMatrix()   
    invertedMatrix <- solve(matrixData, ...)
    x$setInverse(invertedMatrix)
    invertedMatrix
}

# test the functions
# This code has been cribbed from Jules Stuifbergen who posted a version of what
# follows on the discussion forum for the Coursera course, Programming in R, on 
# 13 February 2015.

# generate the test matrix and calculate its inverse via the "usual"
# method (i.e., using solve()).
size <- 1000    # size of the matrix edge
mymatrix <- matrix(rnorm(size^2), nrow = size, ncol = size)
print("Calculating matrix inverse via solve()...")
mymatrix.inverse <- solve(mymatrix)
print("Matrix inverse solution via solve() completed.")

# now solve the matrix via the cache-method; this should take quite a while 
# since its the first time it's invoked
print("Calculating matrix inverse via makeCacheMatrix()...")
special.matrix <- makeCacheMatrix(mymatrix)
special.solved.1 <- cacheSolve(special.matrix)
print("Matrix inverse solution via makeCacheSolution() completed.")

# solve the matrix via the cache-method for a second time; this should be 
# relatively fast
print("Calculating matrix inverse via makeCacheMatrix() using cached value...")
special.solved.2 <- cacheSolve(special.matrix)
print("Matrix inverse solution via makeCacheSolution() using cached value completed.")

# check if all solved matrices are identical; the statement should return TRUE
print(paste("Are inverted matrices identical? ", identical(mymatrix.inverse, special.solved.1) & identical(mymatrix.inverse, special.solved.2)))

