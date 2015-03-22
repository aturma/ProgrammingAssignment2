## makeCacheMatrix has 4 functions defined inside it: set and get which set and retrieve a matrix
## It also has a setinv and getin function which set and retrieve the inverse of the matrix
## I use the as.matrix() function to ensure that even if a vector is entered, it is forced into a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix(...)) {
                print("Setting the matrix")
                mx <- as.matrix(y)
                x <<- mx
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv = matrix(...)) {
                print("Setting the matrix")
                mx <- as.matrix(inv)
                inv <<- mx
                m <<- NULL
        }
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve is a function that takes as input a matrix x of the data type makeCacheMatrix()
## The ... refers to other parameters that the Solve function could use
## This function first checks if the makeCacheMatrix has already set an inverse in which case it returns 
## the inv already set using $setinv. Else it computes the inverse and stores it using $setinv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'        
                inv <- x$getinv()
                if(!is.null(inv)) {
                        message("Getting cached inverse")
                        return(inv)
                }
                inv <- solve(x$get(),...)
                x$setinv(inv)
                inv
}
