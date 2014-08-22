# File created by Stephen Burgin 2014-08-20


# the assignment was to write a pair of functions that create and cache
# the inverse of a matrix and retrieve


# creates a special "matrix" object that can cache its inverse
# im represents the inverse matrix
# m represents the original matrix
makeCacheMatrix <- function(m = matrix()) {
      
      # set the initial object's inverse matrix value to NULL
      im <- NULL
      
      # set up the functions set, get, setInverse, and getInverse that will
      # be accessed by cacheSolve and any other functions that need them
      
      setMatrix <- function(y) {
            
            # set the value of the original matrix to the matrix that
            # was sent in via the variable y
            m <<- y
            
            # clean out the inverse matrix holder
            # that we are using as our "cache" variable
            im <<- NULL
      }
      
      # get the value of the original matrix
      getMatrix <- function() m
      
      # set the value of the inverse matrix
      setInverse <- function(inverseMatrix) im <<- inverseMatrix
      
      # get the value of the inverse matrix
      getInverse <- function() im
      
      # attach all these methods to the object so they can be accessed
      # outside of the makeCacheMatrix function
      list(setMatrix = setMatrix,
           getMatrix = getMatrix,
           setInverse = setInverse,
           getInverse = getInverse)
}


# if the inverse has already been calculated then cacheSolve
# will return the value and skip the computation, if not then it
# computes the inverse of the matrix and returns it
# mo represents the matrix object created by initializing makeCacheMatrix()
# im represents the inverse matrix
# m represents the original matrix
cacheSolve <- function(mo, ...) {
      
      # access the getInverse method on the mo object that was passed in
      # this stores the inverse of the matrix in the local im variable
      im <- mo$getInverse()
      
      # if the inverse matrix value has already been computed and comes
      # back from the getInverse method as NOT NULL, the use the value
      # that was cached
      if(!is.null(im)) {
            
            # tell the user that we are using a cached value
            message("getting cached data")
            
            # return the inverse matrix value
            return(im)
      }
      
      # if the im value came back as NULL, we need to compute the inverse matrix,
      # so we get the original matrix from the matrix object and store it locally
      m <- mo$getMatrix()
      
      # compute the inverse of the matrix and store it in the im variable
      im <- solve(m)
      
      # set the inverse matrix in the cache of the matrix object
      mo$setInverse(im)
      
      # return the inverse matrix we just computed
      im
}
