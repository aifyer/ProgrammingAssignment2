## My submit for Programming Assignment 2. Including 2 function. 
## By working together, they can create a special "matrix" object that can cache its inverse.
## If the inverse object is not calculated, it can calculate and cache it. otherwise, it will retrieve it from chache.


## The first function, makeCacheMatrix creates a list containing a 4 functions to
#set the value of the matrix
#get the value of the matrix
#set the Inverse value of the matrix
#get the Inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix()){
                x <<- y
                inv <<- NULL
        }         
        get <- function() x
        setInverse <- function(inverse){
                inv <<- inverse
        }
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

## The cacheSolve function calculates the inverse of the special matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setInverse(inv)
        inv
}
