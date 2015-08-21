# These functions enable calculating the inverse of a (invertible) matrix and 
# store it to the cache in order to avoid rerunning the calculation every time.
# This saves time, since that calculation can be quite resource-consuming.

# The first function, makeCacheMatrix, is more of a helper function and creates 
# a special "vector", which is really a list containing a function to
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

# If you call this function on a matrix and assign its result to a new variable
# (e.g. "cached"), the respective values will be stored in the lists as laid out
# above.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function, cacheSolve, can be called on the stored results (i.e. 
## "cached", if you follow the example above) of the first function. The first 
## time the function is called, the if-condition is not satisfied and it will compute the inverse of the matrix and
## store it within the environment of "cached". 

## The next time cacheSolve is called on the same object, it will retrieve the 
## inverse via x$getinverse(). Then, the condition of the if-command is
## fullfilled and the function returns the cached inverse along with the message
## "getting cached data".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
