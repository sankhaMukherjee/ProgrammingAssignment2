## Put comments here that give an overall description of what your
## functions do

##############################################################
## Write a short comment describing this function
## This function returns a list containing four 
## functions, which have the matrix `x` and the inverse
## of the matrix `inv` as floating variables to all 
## these functions. This function is called with the
## required value of `x` set (default value is an empty
## matrix). 

## The functions `getValue()` and `getInv()` just return
##  the values `x` and `inv` respectively. 

## The functions `setValue()` and `setInv()` takes an input
##  value and just makes the floating values in their parent 
##  envirinment equal to the one supplied
##
## --------------------------
## Example Execution:
#   for an available matrix `c`
# > c
# [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
# > xx = makeCacheMatrix(c)
##############################################################

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

###########################################################
## Write a short comment describing this function
# 
## cacheSolve() 
##  It first checks to see if a value is present in the 
##    `inv` variable in the environment of the `xx` 
##    class. If the `inv` is `NULL` then it is calculated,
##    and the value of `inv` is set to this calculated 
##    value, otherwise it doesnt do anything. The return value 
##    is simply the value of `inv`
############################################################
# > cacheSolve(xx)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > cacheSolve(xx)
# this is catched
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > c
# [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
#######################################################
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
