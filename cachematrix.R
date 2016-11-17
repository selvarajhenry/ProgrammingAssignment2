## Selvaraj Henry, 11/16/16

## goal
## Implement a method to avoid unnnecessary recompute of matrix inversion


## function to create a special matrix that has the capability to cache the inverse of the matrix
## there are 4 functions included in this function, namely
## set the value of the matrix, get the value of the matrix, set the inverse and get tehe inverse
## this function works in conjuction with cachesolve function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(invrs) m <<- invrs
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## function for efficient matrix inversion
## function checks to see whether the inverse is already computed. if so, it justs returns
## the already computed. If not it calculates the inverse. caches it and resturns the result
## this function works in conjuction with makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## sample usage/test
# r1 = c(1, -1, 1)
# r2 = c(1, 1, 1)
# r3 = c(1, 2, 4)
# m1 = rbind(r1, r2, r3)
# cm1 = makeCacheMatrix(m1)
# cacheSolve(cm1)
## output
#             r1   r2         r3
#[1,]  0.3333333  1.0 -0.3333333
#[2,] -0.5000000  0.5  0.0000000
#[3,]  0.1666667 -0.5  0.3333333