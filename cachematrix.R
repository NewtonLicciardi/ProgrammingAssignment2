## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y)
        {
                x<<-y
                inverse<<-NULL
        }
        get<-function() x
        setm<-function(solve) inverse<<- solve
        getm<-function() inverse
        list(set=set, get=get,
         setm=setm,
         getm=getm)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inverse<-x$getm()
        if(!is.null(inverse))
        {
        message("getting cached data")
        return(inverse)
        }
        m<-x$get()
        inverse<-solve(m, ...)
        x$setm(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
}
