
# makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# create a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  # initiate matrix with NULL
  m<-NULL
  # 
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  # #set the value of cache matrix
  get<-function() x
  setmatrix<-function(solve) m<<- solve 
  # get the value of cache matrix
  getmatrix<-function() m 
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# cacheSolve function creates inverse of matrix created by makeCacheMatrix
# if the inverse has already been calculated then it retrieves the inverse matrix from cache
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}