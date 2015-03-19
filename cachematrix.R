## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setValue <- function(y){ 
        x <<- y
        inv <<- NULL
    }
    
    getValue <- function() x
    getInv   <- function() inv
    setInv   <- function(invVal) { inv <<- invVal }
    
    # We shall return this list of functions
    # Each function has `x` and `inv` as floating 
    # variables, which can be modified by the set
    # functions created abovef
    list(   getValue = getValue,
            setValue = setValue,
            getInv   = getInv,
            setInv   = setInv
        )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    # First read the cached value
    inv <- x$getInv()
    
    # If this exists, just return it.
    if (!is.null(inv)) { message("this is catched"); return(inv) }
    
    # When it doesnt exxist, compute it
    data <- x$getValue()
    inv  <- solve(data, ...) # Hopefully the user will supply other meaningful values to solve
    
    # store it
    x$setInv( inv )
    
    # Finally, return this computed inverse
    inv
    
}
