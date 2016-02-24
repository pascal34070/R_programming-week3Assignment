##This function create a special "matrix" that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        get <- function() m
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##This function computes the inverse of the special “matrix” returned 
##by makeCacheMatrix above
cacheSolve <- function(m, ...) {	
        inv <- m$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- m$get()
        inv <- solve(data, ...)
        m$setInverse(inv)
        inv
}