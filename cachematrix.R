## These group of functions computes the inverse of a matrix 
## and caches the result if necessary to be recomputed again.
##
## Usage:
##        m<-makeCacheMatrix(x = matrix())
##        cacheSolve(m)


## The functions in makeCacheMatrix put (and return)
## into (from) the cache the Matrices and their inverses.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  
  # Function to Cache the Matrix
  set<-function(y){
  x<<-y
  inv<<-NULL
}

# Function to get the Matrix
get<-function() x

# Function to Cache the Inverse Matrix
setinverse<-function(solve) inv<<- solve

# Function to get the Inverse Matrix
getinverse<-function() inv

list(set=set, get=get,
   setinverse=setinverse,
   getinverse=getinverse)
}

## The cacheSolve function receives a matrix and looks
## into the cache if its inverse has been calculated.
## If so, gets the result from the cache. If not calculates 
## the inverse and puts the result into the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # Verify if the Inverse is cached and return it
    inv<-x$getinverse()
    if(!is.null(inv)){
      print(' return cached inverse')
      return(inv)
    }
    
    # Cache the new matrix
    matrix<-x$get()

    # Calculate the inverse of the Matrix    
    inv<-solve(matrix, ...)

    # Cache the inverse and return it 
    x$setinverse(inv)
    inv		
}
