## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function is creating a matrix, which will be cached.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        ## This is where the inverse of the matrix (inv) is stored. 
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setmat<-function(solve) inv<<- solve
        getmat<-function() inv
        list(set=set, get=get,
         setmat=setmat,
         getmat=getmat)

}


## This function will calculate the inverse of the matrix or fetch it from the cache

cacheSolve <- function(x, ...) {
        inv<-x$getmat()
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        mat<-x$get()
        inv<-solve(mat, ...)
        x$setmat(inv)
         ## print the output of the solved (inverse) matrix
        inv
}
