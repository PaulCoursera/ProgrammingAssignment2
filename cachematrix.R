## Calculate the inverse of the matrix ().

## If the matrix inverse has been calculated allready,   
## return it from cache. This avoids recalculation
##
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Calculate the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the data and sets the inverse value of the matrix in the 
## cache via the setmatrix function.
##

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  
  matrix<-x$get()
  m<-solve(matrix, ...)
  
  ##
  x$setmatrix(m)
  m
}
