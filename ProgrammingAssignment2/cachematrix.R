## Programming Assignment #2
##Caching the inverse of matrix

## Create a special matrix object

makeCacheMatrix <- function(x = matrix()) { ##define the function argument as matrix on default
  i<-NULL                                   ##set i, our inverted matrix, which is calculted in cacheSolve, to NULL
  set<-function(y){
    x <<- y                                 ##assign the input arg to the object x, which is from "parent" environment
    i <<- NULL                              ##assign value NULL to the object i from "parent" environment
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function () i
  list(set = set, get = get,                ##assign the functions as elements of the list, so we can call it using $ operator
       setinv = setinv,
       getinv = getinv)

}


## Compute the inverse of a special matrix object

cacheSolve <- function(x, ...) {
  i <- x$getinv()                          ##call the function on the input object                
  if(!is.null(i)){                         ##check is it's NULL or else
    message("getting cached data")
    return(i)                              ##if not NULL, return the valid inverse matrix
  }
  data <- x$get()                          ##if NULL, calculate the inverse matrix of x
  i <-solve(data, ...)
  x$setinv(i)
  i                                         ##return a matrix that is the inverse of 'x'
}
