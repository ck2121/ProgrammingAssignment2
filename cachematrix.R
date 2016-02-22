

##Function caches the inverse of a matrix so that it can be easily retrieved

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
      
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(
      set = set,
      get = get,
      setinverse=setinverse,
      getinverse=getinverse
    )
}



## Calculate the inverse of the matrix utilizing the previously created function.
##If a cached result is available, it will use that

cacheSolve <- function(x, ...) {
    m<- x$getinverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    z <- x$get()
    m<- solve(z,...)
    x$setinverse(m)
    m
}
