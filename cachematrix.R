
##  The first function, `makecacheMatrix` creates a inverse of Matrix, which is
## really a list containing a function to

#     1.  set the value of the matrix
#     2 . get the value of the matrix
#     3.  set the value of the Inverse
#     4.  get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setInverse<-function(Inverse) Inv <<-Inverse
    getInverse<-function()Inv
    list(set=set,get=get,
         setInverse=setInverse,
         getInverse=getInverse)

}


## The following function calculates the Inverse of the Matrix
## created with the above function. However, it first checks to see if the
## Inverse has already been calculated. If so, it `get`s the Inverse from the
## cache and skips the computation. Otherwise, it calculates the Inverse of
## the data and sets the value of the Inverse in the cache via the `setInverse`
#   function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inv<-x$getInverse()
    if(!is.null(Inv)){
        message("Getting Cache Data return")
        return(Inv)
    }
    data<-x$get()
    Inv<-Inverse(data, ...)
    x$setInverse(Inv)
    Inv
}
