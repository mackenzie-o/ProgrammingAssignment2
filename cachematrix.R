
## Write a short comment describing this function
#Given a matrix, creates an object that stores the matrix,
#the inverse of the matrix (NULL if not set), and getting
#and setting functions for both variables.

makeCacheMatrix <- function(x = matrix()) {
    #inverse is null by default
    inverse <- NULL
    #if the data is reset, reset the inverse to null
    set <- function(new_matrix){
        x <<- new_matrix
        inverse <<- NULL
    }
    #return the matrix
    get <- function() x
    #set the inverse of the matrix
    setinverse <- function(new_inverse) inverse <<- new_inverse
    #return the inverse
    getinverse <- function() inverse
    
    #return a list of functions that can be preformed
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
#Given a makeCacheMatrix object, return the inverse of the
#matrix, by either retrieving the inverse if has already 
#been calculated, or calculating and storing it if it has not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    #if the inverse is not null
    if(!is.null(inverse)){
        #just return inverse
        message("returned cache")
        return(inverse)
    }
    #otherwise, calc the inverse...
    message("calculated inverse")
    mtx <- x$get()
    inverse <- solve(mtx)
    #... store the inverse...
    x$setinverse(inverse)
    #... and return it
    inverse
}
