## 2 functions are included in the file:
## makeCacheMatrix: Creates a list which makes it possible to
## cache an inverse matrix for a given matrix input
## cacheSolve: Calculates the inverse of a matrix for a given CacheMatrix input. 
## If there's an inverse matrix cached, it only returns the cached matrix.

##Creates a CacheMatrix of the class list with the ability so save an inverse matrix for a given
##matrix x. 
makeCacheMatrix <- function(x = matrix()) {
    ##Init inverse
    inverse <- NULL
    
    ##Set function sets internal x value to given matrix
    set <- function(y){
      x <<- y
      inverse <<- NULL
    }
  
    ##Returns internal x matrix
    get <- function() x
    ##Caches inverse matrix
    setinverse <- function(inv) inverse <<- inv
    ##Returns cached inverse matrix
    getinverse <- function() inverse
    
    ##Returns list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##Checks if there's a cached inverse matrix to x. If not, it calculates the inverse of x
##and caches it. Returns the inverse of x in both cases.
cacheSolve <- function(x, ...) {
  
    #Checks if an inverse already exists
    inverse <- x$getinverse()
    if(!is.null(inverse)){
      message("getting cached data")
      return(inverse)
    }
    #If null, set and calculate the inverse
    matrix <- x$get()
    #Make an identity matrix (assuming square matrix)
    i <- diag(ncol(m))
    #Calculate inverse
    inverse <- solve(m,i)
    #Set cached inverse
    x$setinverse(inverse)
    #return inverse
    inverse
}
