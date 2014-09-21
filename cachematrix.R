## First function create a special matrix cache its inverse
## Second function calculate the inverse of matrix created in the first and cache it

## Two arguments are used for this function. First one give data for the matrix and second one dimension of the square matrix.

makeCacheMatrix<-function(x,n){
        inver<-NULL
        y<-matrix(x,n)
        set<-function(z,m){
                y<<-matrix(z,m)
                inver<<-NULL
        }
        get<-function()y
        setinver<-function(solve)inver<<-solve
        getinver<-function()inver
        list(set=set,get=get,setinver=setinver,getinver=getinver)
}       

## calculate inerse of the matrix and cache it

cacheSolve <- function(x, ...) {
        inver <- x$getinver()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinver(inver)
        inver
}
