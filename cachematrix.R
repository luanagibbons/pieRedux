## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# creates a makeCacheMatrix with two attributes -- the input matrix and an 
# inverse matrix. The accessor and mutator functions are also defined.
# in no inverse is cached NULL is returned.
# example usage: aMatrix <- makeCacheMatrix(matrix(1:4,2,2)) 


makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set<- function(y) {
    x <<- y
    invX <- NULL
  }
  get <- function() x   #data accessor
  setinverse <- function(inverse) invX <<- inverse # inverse mutator
  getinverse <- function() invX   # inverse accessor
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #named so can access with $
}


## Write a short comment describing this function
#returns a cached inverse if it exists, otherwise retrieves original matrix, 
#inverts and caches the inverse matrix. 
#Note: the function takes an object of type makeCacheMatrix.
#example usage: 
#aMatrix <- makeCacheMatrix(matrix(1:4,2,2))
#cacheSolve(aMatrix)

cacheSolve <- function(x, ...) {
  invX <- x$getinverse()     
  if(!is.null(invX)) {
    message("getting cached inverse matrix")
    return(invX)
  }
  #otherwise invert and cache matrix
  data <- x$get()
  invX <- solve(data)
  x$setinverse(invX)
  invX
}
