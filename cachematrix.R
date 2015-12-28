## These two functions work together to keep track of whether the matrix inverse has already been calculated by setting
## m equal to null if the set() function, defined by makeCacheMatrix, is called, which tells cacheSolve whether to
## calculate the inverse or retrieve it from the cache

## This function creates 4 sub functions which are used to set the matrix and store
## the inverse calculated by cacheSolve, as well as to determine whether the inverse has already been cached

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function checks if the matrix inverse has already been calculated and if not, calculates it

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)){
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}

