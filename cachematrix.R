## Assingment 2 functions: Lexical Scoping

## makeCacheMatrix function
## use this to create an object which groups a data vector with functions for storing/accessing cached metadata about it
##  set : store matrix in parent object
##  get : get matrix from parent object
##  setinverse : cache the inverse of parent object's matrix in parent object
##  getinverse : get the cached inverse matrix from parent object
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL   ## this is my inverse matrix object
    set <- function(y) {
        x <<- y
        i <<- NULL   ##NULLS any previous inverse matrix
    }
    get <- function() x
    setinverse <- function(y) i <<- y
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function
## use this to return a matrix that is the inverse of a passed in makeCasheMatrix object
## key benefit is inverse is calculated once and stored / returned thereafter without recalculating it!
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    ## else...the inverse was NULL, so calculate it (and set it)
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}


## -----this is my test script---------------
## create a cachedMatrix object and solve its inverse
##a <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
##m <-a$get()
##message("here's the original matrix")
##print(m)
##im <-a$getinverse()
##message("...and its original inverse")
##print(im)
##im <-cacheSolve(a)
##message("here's cachesolve returned")
##print(im)
## do it again to see message that it was cached
##im <- cacheSolve(a)
##message("here's cachesolve returned the second time")
##print(im)
