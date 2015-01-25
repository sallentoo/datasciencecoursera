## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix<-function(x=matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL 
      }
      get <- function() x
      setinv <- function(inv) m <<-inv
      getinv <- function() m
      list(set=set, get=get,
           setinv=setinv,
           getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. It the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x) {
      ## Return a matrix that is the inverse of 'x'
      
      start_time<-Sys.time()
      
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            
            end_time<-Sys.time()
            time_duration<-end_time-start_time
            print(time_duration)
            
            return(m)
      }
      
      data <- x$get()
      m <- solve(data)
      x$setinv(m)
      
      end_time<-Sys.time()
      time_duration<-end_time-start_time
      print(time_duration)
      
      m
}