## Put comments here that give an overall description of what your
## functions do
## The functions defined here allow you cache the inverse matrix for given matrix variables 
## and reuse in future if required

## Write a short comment describing this function
## Function makeCacheMatrix returns a list of functions that can be used to manapulate the input matrix variable 

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setinv <- function(inv) i <<- inv
                getinv <- function() i
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        }
        


## Write a short comment describing this function
## Function cacheSolve returns the inverse matrix of input matrix x. If its inverse matrix is available
## in the memory, it can be retrieved directly from memory. Otherwise, the function will calculate
## its inverse matrix using solve function.
cacheSolve <- function(x, ...) {
  
                i <- x$getinv()
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }
                data <- x$get()
                i <- solve(data, ...)
                x$setinv(i)
                i
        }

# Try the first matrix m1 
m1 <- matrix(1:4, nrow=2, ncol=2)
mm <- makeCacheMatrix(m1)
# Cache the inverse matrix for m1
cacheSolve(mm)

# Try a different 3:3 matrix again
n1 <- matrix(c(2,4,9,1,7,10,12,3,6), nrow=3, ncol=3)
nn <- makeCacheMatrix(n1)
# Cache the inverse matrix for n1
cacheSolve(nn)

# Repeat cacheSolve to test if the inverse matrix has been cached.
cacheSolve(mm)
cacheSolve(nn)
