## caching the inverse of matrix


## makeCacheMatrix returns a list containing a function to 
## set the Matrix 
## get the Matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
InvA <- NULL
        set <- function(Y) {
                x <<- Y
                InvA<<- NULL
        }
        get <- function() x
        setsolve <- function(solve) InvA <<- solve
        getsolve <- function() InvA
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve computes the inverse matrix of the special "vector" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matix and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        InvA <- x$getsolve()
        if(!is.null(InvA)) {
                message("getting cached Inverse")
                return(InvA)
        }
        Mat <- x$get()
        InvA <- solve(Mat, ...)
        x$setsolve(InvA)
        InvA
}
