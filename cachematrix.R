## The cachematrix.R file contains two functions, makeCacheMatrix() and cachesolve(). 
## 1) makeCacheMatrix() creates an object that stores a matrix and its inverse. 
## 2) cachesolve() requires an argument that is returned by makeCacheMatrix() to retrieve the inverse matrix
##    from the cached value that is stored in the object environment of the makeCacheMatrix() function.


# makeCacheMatrix() creates a set of functions
# and returns the functions within a list to the parent environment.
# It also includes two data objects, x and inv_x.

#set -> sets the original input matrix in the parent environment
#get -> retrieves the original input matrix in the parent environment
#setinverse -> sets the inverse matrix in the parent environment
#getinverse -> retrieves the inverse matrix from the parent environment
# x -> input matrix
# inv_x -> inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
inv_x<-NULL
    set<-function(y){
        x<<-y
        inv_x<<-NULL
    }
    
    get<-function() x
    
    setinverse<-function(solve) inv_x <<- solve
    
    getinverse <- function() inv_x
    
    list(set= set, get= get, setinverse=setinverse, getinverse=getinverse)
}



# cacheSolve() calculates and stores the inverse of a matrix argument of the type makeCacheMatrix.
# Required compliment of the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    inv_x<-x$getinverse()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setinverse(inv_x)
    inv_x
}

