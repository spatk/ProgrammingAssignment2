# ---------------------------------------------------------------
## R Program (ver.3.1.2) Matrix inversion using Caching Approach 
## .....Lexical Scoping......
## Matrix is assumed to be square and has finite inversion value
# ---------------------------------------------------------------
# Usual Matrix operations can do Matrix inversion but could be computationally 
# costly particularly for large matrices (>10000 x 10000).
# Caching the inverse of a matrix rather than compute it repeatedly is a cost # effective approach to reduce computaitonal time.
#
# The functions written to cache the inverse of a matrix.
#    makeCacheMatrix: This function creates a special "matrix" object that can cache its #    	inverse.
#
#    cacheSolve: This function computes the inverse of the special "matrix" returned by # 	makeCacheMatrix above. If the inverse has already been calculated (and the matrix has #	not changed), then the cachesolve should retrieve the inverse from the cache.



# Function to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}  


## Calculate the inverse of cached matrix
#
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(m, ...)
        x$setinv(m)
        m
}
#
