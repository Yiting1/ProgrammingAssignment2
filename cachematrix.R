## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverseOfX = solve(x)

        m <- NULL
        set <- function(y) { #set the value of the matrix
                x <<- y
                m <<- NULL
        }

        get <- function() x  #get the value of the matrix

        setInverse <- function(inverseOfX){ #set the inverse of the matrix
                m <<- inverseOfX
        }

        getInverse <- function() m  #get the inverse of the matrix
        #generate a list of cache values of the matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseOfX <- x$getInverse()   #get the value of the inverse of the matrix
        if(!is.null(inverseOfX)) {   #check if matrix has been cached and skip the statements below
                return(inverseOfX)
        }
        
        data <- x$get()  #get the matrix value and assign it to a variable
        inverseOfX <- solve(data)  #find the inverse of the matrix using the built-in R solve function
        x$setInverse(inverseOfX)  #set the inverse of the matrix
        return(inverseOfX)   #return the inverse of the matrix value
}
