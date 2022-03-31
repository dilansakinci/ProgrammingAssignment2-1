#Coursera R programming assignment for week three
#the function "makeCacheMatrix" creates a special matrix object which can cache its inverse.
#the function "cacheSolve", on the other hand, gets the cache data and inverse it.

makeCacheMatrix <- function(x= matrix()){
        inv <- NULL
        set <- function(y){
          x <<- y
          inv <<- NULL
        }
        get <- function()  x
        setinverse <- function(inverse) 
          inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x,...) {
  inv <- x$getinverse ()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
}
