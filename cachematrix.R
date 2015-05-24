## This programm is composed by a pair of functions that cache the inverse of a matrix


## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }

    get <- function() x
        
    setInverse <- function(inverse){    
        matrixInverse <<- inverse
    }
    
    getInverse <- function() {
        matrixInverse
    }
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    inverseM <- x$getInverse()
    
    if(!is.null(inverseM)){
        
        message("Getting the cached inverted matrix")
        return (inverseM)
        
    }else{
        
        message("Sorry no cached data,we'll have to calculate the inverse of this matrix")
        matrix <- x$get()
        inverseM <- solve(matrix)
        x$setInverse(inverseM)
        return (inverseM)
        
    }
    
}
