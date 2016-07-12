## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  if(dim(x)[1]!=dim(x)[2]){
    message("this is not a square matrix")
    return()}
  m<-matrix(nrow=dim(x)[1],ncol=dim(x)[1])
  set<-function(y){
    x<<-y
    if(dim(x)[1]!=dim(x)[2]){
      message("this is not a square matrix")
      return()}
    m<<-matrix(nrow=dim(x)[1],ncol=dim(x)[1])
  }
  get<-function(){x}
  getinverse<-function(){m}
  setinverse<-function(data){m<<-data}
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(length(m[!is.na(m)])>0){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinverse(m)
  m
}
