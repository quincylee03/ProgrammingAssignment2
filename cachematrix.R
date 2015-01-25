## makeCAcheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {     ## function with argument x that is a matrix
        m <- NULL                               ## set variable m to NULL
        set <- function(y) {                    ## set is a function within makeCacheMatrix, x takes y in outside envt, m is NULL in outside envt
                x <<- y
                m <<- NULL
}
get <- function() x # no arguments for function, just returns x
setinverse <- function(solve) m <<- solve
getinverse <-function() m
list(set= set, get = get, setinverse= setinverse, getinverse= getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       m<- x$getinverse()
       if(!is.null(m)) {        ##if there is data i m, then just return m
               message("getting cached data")
               return(m)
       }
       data <- x$get()          ##if the returne statement is faslse, set data to equal get from makeCacheMatrix
       m <- solve(data)         ## find the inverse of matrix using solve 
       x$setinverse(m)
       m
               ## Return a matrix that is the inverse of 'x'of makeCacheMatrix()
}
