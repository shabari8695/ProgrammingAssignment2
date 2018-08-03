## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function makeCacheMatrix is used for the following purpose:
#1.Set value of input matrix
#2.Get value of the above mentioned matrix
#3.Set value of the inverted matrix
#4.Get value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        #inv is the inverted matrix and is set to NULL initally
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInv <- function(inverse) inv <- inverse
        getInv <- function() inv
        
        list(set = set,get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        
        if(!is.null(inv)){
                message("Getting cached inverted matrix")
                return(inv)
        }
        
        mat <- x$get()
        inverse <- solve(mat,...)
        x$setInv(inverse)
        inv
}
