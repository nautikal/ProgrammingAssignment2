## makeCacheMatrix and cacheSolve are functions which, together,
## calculate the inverse of a matrix and cache it.
## If the original matrix is unchanged, calling the cacheSolve
## function will return its cached inverse without it needing
## to be recalculated.


## The user sets the matrix to be cached using the makeCacheMatrix
## function. The function creates a list of four functions which the 
## user should assign to a variable (to be used with cacheSolve).

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function checks to see if the matrix inverse has
## already been calculated and stored as "i". If so, it returns 
## the inverse matrix. Otherwise, cacheSolve calculates the inverse 
## and caches it for future recall. 

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)){
         message("getting cached data")
         return(i)
     }
     matrix <- x$get()
     i <- solve(matrix, ...)
     x$setinverse(i)
     i
}