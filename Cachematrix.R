# my function with an overall description and what it does
# makeCachematrix creates a matrix object that cache its inverse
makeCachematrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
         get <- function() x            #gets the inverse of the x-matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
# computes the inverse of the makeCachematrix
# if the matrix does not change and the inverse has already been computed it
# will reclaim the inverse from the cache
Cachesolve <- function(x, ...)    # gets the cache data 
     { inv <- x$getInverse()          #returns the inverse of x
     if(!is.null(inv))            # check whether inverse is null
      { message ("getting cache data")
       return(inv) }          # returns inverse value
  
        data <- x$get
        inv <- solve(data, ...) # computes inverse values
        x$setInverse(inv)
        inv                  # returns an inverse matrix of x    
}