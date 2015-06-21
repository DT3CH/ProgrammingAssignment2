## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment

makeCacheMatrix <- function(x = matrix()) {
    
    ## create a special matrix that returns a list of attributes
    
    
    ## set inverse of matrix to null
    i <- NULL
    # use the '<<-' operator to set value of object in different environment
    set = function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## create functions that populate the list that creates the matrix
   
    # get returns the matrix
    get<-function() x
    # set_inverse sets the inverse of the matrix frome the solve() function in cacheSolve
    set_inverse <- function(solve) i <<- solve
    # get_inverse returns the inverse from the cache or calculation in cacheSolve
    get_inverse <- function() i
    # list the names  of functions/methods that create matrix
    list(set = set, get = get, set_inverse=set_inverse, get_inverse=get_inverse)
    
    ##created matrix should return elements of the matrix with x$get command
    ## and return inverted matrix with the x$get_inverse command
    

}

cacheSolve <- function(x, ...) {
    ## call function that takes object 'x' and assign 'i' to the value of x$get_inverse
    i <- x$get_inverse()
    ## if the matrix contains an inverse cached then return the cached inverse matrix
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## otherwise collect data elements from matrix object x sent to function with x$get
    ## use solve() to find the inverse and assign to i and return
    
    elements <- x$get()
    i <- solve(elements, ...)
    
    ## set the inverse of matrix in list of object 'x' using the x$set_inverse(i) command
    x$set_inverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    
    return(i)
    
}
