## Functions for testing lexical scoping
##
## Testing: 
## 1. Make a square matrix with data, pass it as argument  
##    for function makeCacheMatrix and omit result to object. 
## 2. Pass this result object as argument for function cacheSolve
##    (the result is inversed matrix).
## 3. If you call the function cacheSolve with the same object 
##    the second time, then you get cached data
## 


## The function initializes objects and defines functions, 
## returns the object that contains pointers to functions, 
## that are within the makeCacheMatrix() environment after the function ends

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
          x<<-y
          m<<-NULL
      }    
      get<-function() x
      setinverse<-function(solve) m<<-solve
      getinverse<-function() m  
      
      list(set=set,get=get,
           setinverse=setinverse,
           getinverse=getinverse)
        
   }


## Function passes object of type makeCacheMatrix, 
## uses functions which are defined in makeCacheMatrix enviroment
## If there are cashed inversed matrix data then prints message and returns,
## otherwise calculates anew and no message will be printed 

cacheSolve <- function(makeCacheMatrix.object, ...) {
         m<-makeCacheMatrix.object$getinverse()
        if(!is.null(m)) {
            message('now getting cached data')
            return (m)
        }
        data<-makeCacheMatrix.object$get()
        m.calculated<-solve(data)
        makeCacheMatrix.object$setinverse(m.calculated)
        m.calculated
}
