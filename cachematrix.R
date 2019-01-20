## MakeCacheMatrix sets the values of matrix, gets the values of matrix,
##sets the value of inverse and gets the values of inverse
##cacheSolve computes the inverse the of matrix returned by makeCacheMatrix,
##if inverse is already calculated then it retrieves the inverse from the cache

## MakeCacheMatrix create a special matrix. 

makeCacheMatrix<-function(x=matrix()) {
  m=NULL
  set<-function(y) {
    x<<-y
    m=NULL
  }
  get<-function() x
  setinverse<-function(solve) m<<-solve
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##CacheSolve computes the inverse

cacheSolve<-function(x,...) {
  m=x$getinverse()
  if(!is.null(m)) { ##if inverse is already calculated then its retrieves the cached data
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}

        

