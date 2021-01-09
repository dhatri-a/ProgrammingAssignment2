## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#             If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
    inv_matrix <- NULL                                             #initializing inverse matrix to be NULL        
    set <- function(y) {                                           # given input y, values of matrix are set
             x <<- y
             inv_matrix <<- NULL
    }       
    get <- function() x                                            # get values of matrix       
    setInv <- function(inverse) inv_matrix <- inverse              # set values of inverse matrix
    getInv <- function() inv_matrix                                # get values of inverse matrix
        
    list(set = set, get = get, setInv = setInv, getInv = getInv)        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
     inv_matrix <- x$getInv()                                      # get inverse of x        
     if(!is.null(inv_matrix)) {                                    # check if matrix is alrady inverse, to return it as is
             message("getting cached data")
             return(inv_matrix)
     }
     data <- x$get()                                               # otherwise, solve for inverse matrix
     inv_matrix <- solve(data, ...)
     x$setInv(inv_matrix)                                          # set inverse matrix
        
     ## Return a matrix that is the inverse of 'x'
     inv_matrix
       
}
