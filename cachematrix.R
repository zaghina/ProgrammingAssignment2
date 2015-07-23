## I created two functions: makeCacheMatrix and cacheSolve
## The first will store an input matrix and make sure that the class of the input is a matrix
## The second will calculate the inverse of the matrix stored via the first function
## If this process has been performed before, a message will display stating that the inverse is
##  being recalled rather than re-calculated, aka. it is "cached data".
## To test these two functions:
    ##1.Store the matrix in an object (let's call it "Object1")
    ##2.Store the output of makeCacheMatrix("your object's name") in another object (e.g."Object2")
    ##3.Apply cacheSolve function on the second object's name ("Object2") --> you will get the inverse of the first object ("Object1")
    ##4.Re-apply step #3 and you will also get the inverse with the first object ('Object1")
      ##However, you will also get a message stating that the inverse is being recalled
## To reset data in the first function, use Object2$set("New Matrix")


makeCacheMatrix <- function(x = matrix()) {
   x<- as.matrix(x)
  
   i <- NULL
  
       set<- function(y){
         x <<- y
         i <<- NULL
        }
  
       get<- function()x
  
       setinverse<- function(inverse) i <<- inverse

       getinverse<- function() i
  
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Kindly see the beginning of this document for information about this function

cacheSolve <- function(x, ...) {
   i <- x$getinverse()
    if(!is.null(i)){
       message("getting cached data")
       return(i)
      }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
