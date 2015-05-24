
## Finding the inverse of a matrix can be a time-consuming computation in R, especially if you 
## have to repeat the function in loop over large matrices. To make this a more efficient process, 
## I have created two functions; makeCacheMatrix() and cacheSolve(). This two function process is 
## vastly more efficient because it allows R to only compute the inverse of a matrix once and cache 
## (save) the value. If it needs to recall the inverse of a matrix it has already computed then it 
## can simply look up the cached value instead of re-computing the function. 

## makeCacheMatrix() takes a matrix or matrix like input and computes the inverse of that matrix 
## using the solve() function. However, instead of printing the value of the inverse matrix into 
## the consul, the returned value is cached (saved) to be retrieved later. To do this, I have 
## created the objects; set, get, setinv and getinv. The set object stores the data of the matrix 
## being inputted into the makeCacheMatrix() function and the get object is essentially a flag so 
## R can easily look to match the new input data to the cached input data. The setinv object 
## computes the sovle() function to find the inverse of the matrix being inputed while the getinv 
## object acts as a flag or marker so the value of the inverse can easily be recognized and retrieved
## from the cache. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve() is a special function that first looks up the cached values from makeCacheMatrix 
## to see if the value of inverse matrix(m) has already been computed. If it has, cacheSolve simply 
## returns the value of the inverse that was cached by makeCacheMatrix(). If cacheSolve() cannot
## find the inverse value in the cached data, then cacheSolve() will compute the inverse of the 
## matrix using the solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
