## This function creates a special "matrix" object
## that can cache its inverse
## It is a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the inverse of matrix
## 4.  get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        }

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inversed has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the `setsolve`
## function.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setsolve(s)
        s
}
        
        
        
        
        
        
