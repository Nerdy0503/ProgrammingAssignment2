## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a list of four functions (see class(makeCacheMatrix(x)) 
# and summary(makeCacheMatrix(x)) after running this code) described as follows
# "set" function allows to cache the value of the matrix passed to it into var-
# iable x.
# "get" function allows to retrieve that value from cache.
# "setinv" function allows to cache the value of the inverse to some variable 
# myInv
# "getinv" function allows to retriev the value of the inverse that has been 
# cached, myInv

makeCacheMatrix <- function(x = matrix()) {
    myInv <- NULL
    set <- function(y) {
        x <<- y
        myInv <<- NULL
    }
    get <- function() x
    setinv <- function(vInv) myInv <<- vInv
    getinv <- function() myInv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}

## Write a short comment describing this function
# This function takes the output of the previous function as input, checks 
# whether its inverse has already
# been cached. If it hasn't, it gets the value of x that has been cashed and 
# calculatesits inverse and then returns it.
# PS: We can see this by running twice cacheSolve() on the same output of the 
# first function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    myInv <- x$getinv()
    if(!is.null(myInv)){
      message("Getting cached data")
      return(myInv)
    }
  
    mat2binv <- x$get()
    myInv <- solve(mat2binv, ...)
    x$setinv(myInv)
    myInv
}

