
## Description - makeCacheMatrix stores a list with 4 functions: 
## 1. set(y) - that creates and caches the original (passed as an argument) matrix and resets the variable "minv" to NULL ("minv" is used to store the inverted maxtrif by "cacheSolve") 
## 2. get() - that returns the previous cached matrix. It's used by "cacheSolve" to retrieve the original matrix and compute the inverted matrix with solve()
## 3. setinverse(inverse) - that is used to cache the inverted matrix computed by "cacheSolve".
## 4. getinverse() - that returns the cached inverted matrix

makeCacheMatrix <- function(m = matrix()) {
  minv<- NULL
  set<- function (y){
    m<<-y
    minv<<- NULL
  }
  
  get <- function () m
  setinverted<- function (inverted){minv<<- inverted}
  getinverted<- function() minv
  list(set = set, get = get, # the list is made with the 4 functions explained previously
        setinverted = setinverted,
        getinverted = getinverted)

}


## "cacheSolve" receives a list created by "makeCacheMatrix" and returns a cached inverted matrix using getinverted(), a subfunction of "makeCacheMatrix" 
## It computes and stores the inverted matrix if no cached inverted matrix is found.
cacheSolve <- function(m, ...) {
       
  minv <- m$getinverted() # tries to retrieve the inverted matrix 
  if(!is.null(minv)) { # if minv is not NULL it should contain a inverted matrix 
    message("getting cached data")
    return(minv) # returns it here
  }
    data <- m$get() #if minv is NULL we have to retrieve the original matrix
    minv <- solve(data, ...) # and perform the calculation. 
    m$setinverted(minv) # we then cache that value to be used in the future
    minv # outputs the cached inverted matrix 
  }
  
