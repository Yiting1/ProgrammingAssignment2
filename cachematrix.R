## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverseOfX = solve(x)

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x

        setInverse <- function(inverseOfX){
                m <<- inverseOfX
        }

        getInverse <- function() m

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseOfX <- x$getInverse()
        if(!is.null(inverseOfX)) {
                return(inverseOfX)
        }
        
        data <- x$get()
        inverseOfX <- solve(data)
        x$setInverse(inverseOfX)
        return(inverseOfX)
}
