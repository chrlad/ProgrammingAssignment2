## Put comments here that give an overall description of what your
## functions do
## The two function defined here computes the inverse of an invertible matrix and stores it in the cache for future use
## 

## This function  makeCacheMatrix emulates what the mekeVector function was doing.
## It creates a list of functions that
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## The function cacheSolve returns the inverse of a given (invertible) matrix. 
## This function requires that a object has benn created via the makeCachematrix function
## It first checks whether the inverse has already been computed and stored in cache
## If the inverse is available in the cache it returns this value
## if not it calculates the inverse and stores the value in the cache and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

