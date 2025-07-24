

## This function creates a list that contains functions to set the value of matrix,
## get the value of matrix,set the value of inverse, get the value of inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
         x<<-y
         m<<-NULL
    }
    get<- function()x
    setinverse<-function(inverse)inv<<-inverse
    getinverse<-function()inv
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)

}


##This function firstly checks to see if the inverse has been calculated and cached.
##If so ,it gets the value of inverse from the cache and don't calculate the inverse.
##Otherwise, it calculates the inverse of the matrix and sets the value of inverse in 
##the cache via the set mean function.

cacheSolve <- function(x, ...) {
       inv<-x$getinverse()
       if(!is.null(inv)){
          message("getting cached data")
         return(inv)
       }
       data<-x$get()
       inv<-solve(data,...)
       x$setinverse(inv)
       inv
}
