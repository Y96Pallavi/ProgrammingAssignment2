## There are two functions makeCacheMatrix and cacheSolve. The fist function craetes an object that can cache its inverse
## The second function computes the inverse only if the inverse does not exist in cache


## makeCacheMatrix is a function that has 4 parts set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
  #Setting inverse to NULL
  inv<-NULL
  #set function which sets x and inverse(inv)
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  #creating get function to get matrix
  get<-function() x
  #Here one can set the inverse for the matrix
  setinverse<-function(inverse) inv<<-inverse
  #Gives the set inverse
  getinverse<-function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The following function calculates the inverse only if the inverse from the previous function is NULL

cacheSolve <- function(x, ...) {
  #Getting the inverse of matrix x
  inv<-x$getinverse()
  #If the inverse is NULL the the condition within if comes out FALSE in such a case we do not go in if
  #instead it calculates the inverse, using the code under the if statement. If inverse did exist earlier
  #then instead of calculating again, the function gets it from cache
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}


