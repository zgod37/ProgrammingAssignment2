# Script to calculate and cache the inverse of a matrix

# Returns a re-usable CacheMatrix object containing four functions
# Contains get/set methods for both the original input matrix and its inversion

makeCacheMatrix <- function(a = matrix()) {

    #the inverse of input matrix
    the_inverse <- NULL
    
    #create getter/setters
    set <- function(b) {
        a <<- b
        the_inverse <<- NULL
    }
    get <- function() a
    set_inverse <- function(inverse) the_inverse <<- inverse
    get_inverse <- function() the_inverse
    
    # return the CacheMatrix object
    list(set=set, get=get, 
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

# Accepts a CacheMatrix object as described above
# Returns the inverse of the located in the vector
# If matrix has already been inverted, it returns the cached value
# Otherwise it computes the inverse, sets it in the cache and returns it

cacheSolve <- function(a, ...) {

    #check if inverse has already been cached
    the_inverse <- a$get_inverse()
    if(!is.null(the_inverse)) {
        message("getting cached inverse")
        return(the_inverse)
    }
    
    #otherwise compute it and cache the result in CacheMatrix
    data <- a$get()
    the_inverse <- solve(data, ...)
    a$set_inverse(the_inverse)
    
    #return the computed inverse
    the_inverse
}
