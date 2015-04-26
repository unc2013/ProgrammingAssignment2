##
# The following two functions are used to cache the inverse of a matrix.
#
# function: makeCacheMatrix --- creates a list containing a function that
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of inverse of the matrix
# 4. gets the value of inverse of the matrix
##

makeCacheMatrix <- function(a = matrix()) {
   inv <- NULL
   set <- function(y) {
       a <<- b
       inv <<- NULL
   }
   get <- function() a
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##
# function: cacheSolve --  assumes that the matrix is always invertible.
# 1. checks if the inverse has already been computed
# 2. if so - gets the result and skips computation
# 3. if not - computes the inverse, sets the value in the cache
##

cacheSolve <- function(a, ...) {
   inv <- a$getinverse()
   if(!is.null(inv)) {
       message("getting the cached data....")
       return(inv)
   }
   data <- a$get()
   inv <- solve(data)
   a$setinverse(inv)
   inv
}