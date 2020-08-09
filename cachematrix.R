## The first function, `makeCacheMatrix` creates a matrix 
## and retun cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
		Inverse <- NULL
        set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) Inverse <<- inverse
        getInverse <- function() Inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This funcation, `cacheSolve` verify matrix inverse availablity in cache,
## If Inverse matrix available in cache than return value from cache else
## evaluate inverse of matrix and share result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		Inverse <- x$getInverse()
        if (!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setInverse(Inverse)
        Inverse
}
