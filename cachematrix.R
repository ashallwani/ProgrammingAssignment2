## The makeCacheMatrix and cacheSolve matrix rapidly compute the 
## inverse of an invertible matrix

## The makeCacheMarix creates a special "matrix", which is really a list
## containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
## define the set, get, setInverse and getInverse functions
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created above
## However, it first checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
