## the 2 functions below create a special object that stores a matrix and
## caches its mean

## The "<<-" operator assigns a value to an object in an environment 
## different from the current one

## MakeCacheMatrix creates a special matrix, a list that 
## contains a function that sets, then gets the value of the matrix, 
## then sets and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {                           ##set value of matrix
                x <<- y
                inverse <<- NULL
        }
        get <- function() x                            ##get value of matrix
        setsolve <- function(solve) inverse <<- solve  ##set value of inverse
        getsolve <- function() inverse                 ##get value of inverse
        list(set = set, get = get, setsolve = setsolve, 
                getsolve = getsolve)
}


## The function below computes the inverse of the special matrix above. 
## First, it checks if the inverse has been created already.
## If yes, it retrieves the inverse from the cache. 
## If not, it computes the inverse and sets the elements of the inverse
## matrix in the cache, using the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(inverse)) {                       ## If inverse in cache,
                message("getting inverse from cache") ## print message and &
                return(inverse)                       ## get inverse from cache
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setsolve(inverse)
        inverse
}
