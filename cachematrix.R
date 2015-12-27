## There are 2 functions. makeCacheMatrix and cacheSolve.  Together, they create
## a special matrix and then caches the inverse of this matix.  This allows for
## quicker program speed as the inverse doesn't have to be recalculated everytime.


## makeCacheMatrix takes a matrix as the input and caches the inverse of the
## matrix.  This function has 4 parts: set, get, setInverse and getInverse.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<- function(y) {
        x<<-y
        m<<-NULL
    }
    get <-function() x
    setInverse<-function(solve) m<<-solve
    getInverse<-function() m
    list (set=set, get=get, setInverse= setInverse, getInverse=getInverse)
}



## cacheSolve checks to see if the inverse is already cached, and if it is
## returns the cached value.  If the inverse doesn't exist, we set the inverse
## with the setInverse funtion.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setInverse(m)
    m
    
}
