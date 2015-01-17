## This function makes a matrix with a cached inverse

makeCacheMatrix <- function(x = matrix()) {
        ## x caches the matrix itself
        
        inv <- NULL # the inversion cache
        
        ## get and set matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() {
                x
        }
        
        ## get and set inversion
        setinv <- function(inversion) {
                inv <<- inversion
        }
        getinv <- function() {
                inv
        }
        
        ## return the functions
        list(set = set, get = get,
                            setinv = setinv, getinv = getinv)
}


## This function inverses a matrix
## made by the last function, checking
## the cache for a shortcut

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # read the inverse cache
        inverse <- x$getinv()
        
        ## check for validity and creat inv if needed
        if(is.null(inverse)) {
                matrix <- x$get()
                inverse <- solve(x, ...)
                x$setinv(inverse)
        }
        
        ## return value
        inverse
}
