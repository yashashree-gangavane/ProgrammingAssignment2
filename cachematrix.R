## Finds inverse of matrix and stores in cache

## Sets and initializes the matrix

makeCacheMatrix <- function(x = matrix())  {
    inv <- NULL
    setMatrix <- function(y)    {
        x <<- y
        inv <<- NULL
    }
    getMatrix <- function() x
    setInv <- function(val)    inv <<- val 
    getInv <- function()    inv
    list (setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)
}

## Finds and returns the inverse of matrix

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) 
    }
    data <- x$getMatrix()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
