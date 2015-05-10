## cachematrix.R
## Contains two functions to cache the inverse of a matrix:
##      makeCacheMatrix()
##      cacheSolve()
##
## Example usage:
##    >  m<-matrix(rnorm(9),3,3)
##    >  x<-makeCacheMatrix(m)
##    >  xi<-cacheSolve(x)
##
## where xi is the inverse of the matrix m 



## makeCacheMatrix(x = matrix())
##
## Creates a list of functions specific to a
## given matrix that allows its inverse
## to be cached
##
##    Input: a square matrix x
##    Output: a list containing functions to
##             1. set the value of the matrix
##             2. get the value of the matrix
##             3. set the value of the inverse
##             4. get the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve(x, ...)
## Returns the inverse of the matrix pointed to
## by the list of functions contained in x.  If
## the inverse has not been computed previously,
## the inverse is computed using solve().  If
## the matrix has been computed previously, the
## function returns the cached matrix inverse.
##
##    Input: the list of functions x which is the
##           output of makeCacheMatrix(m) where m
##           is a square matrix
##    Output: the inverse of the matrix m
##

cacheSolve <- function(x, ...) {
        ## Check to see if the inverse has been
        ## computed
        I <- x$getinverse()
        if(!is.null(I)) {
            ## If so, retrive cached inverse 
                message("getting cached data")
                return(I)
        }
        ## If not, compute inverse
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I
}
