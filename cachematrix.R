##  makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setmatrix <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setmatrix = setmatrix,
             getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        tryCatch({i <- solve(matrix, ...)
                  }, 
                 error = function(i) {
                         message("Error:")
                         message(i)     
                 },
                 warning = function(i) {
                         message("Warning:")
                         message(i)  
                 },
                 finally = {
                         x$setmatrix(i)
                 } )
        i
        ## Return a matrix that is the inverse of 'x'
}
