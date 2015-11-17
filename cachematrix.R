## Put comments here that give an overall description of what your
## functions do

# It is really awesome! I self-learned R before for ML competitions,
# however, I did not even notice this scoping problem. Thank you for
# your guidance!

# If not using new operator `<<-`, get() will always return NA since
# lexical scoping. I really wonder the implementation in details for 
# this machanism.

## Write a short comment describing this function
# Operator `<<-` is usually used in function closure, which prevents
# the lexical scoping problem.
makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL 
    set <- function(m) {
        mat <<- m
        inv <<- NULL
    }
    get <- function() mat
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## Write a short comment describing this function
# Nothing special. Check the value in function before return it.
cacheSolve <- function(x, ...) {
    inv <- x$getsolve()
    if (!is.null(inv)) {
        message('getting cache data')
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setsolve(inv)
    inv
}
