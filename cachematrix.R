## The first function "makeCacheMatrix" creates the matrix object.
# The second function "cacheSolve" computes the inverse matrix.
# If there has been no change in the input-matrix, the returned value, i.e. the inverse matrix is not re-calculated but retrieved at cached data instead.



## The function below: makeCacheMatrix creates a matrix object, which can cache/retrieve the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, 
	get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## This function computes the inverse of the matrix that is the output of makeCacheMatrix. If the input-matrix is new/unknown the inverse matrix is calculated. 
#If the input-matrix is unchanged the inverse matrix is retrieved as "cache" data, and thus not recalculated.


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

# testing the functions

test1<-makeCacheMatrix() 
test1$set(matrix(2:5,2,2)) # defines a 2 by 2 matrix with the values 2:5, and sets the matrix to "test1"

# typing: cacheSolve(test1)
#  returns: 
##      [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1


# retyping: cacheSolve(test1)
# returns:

## getting cached data
##       [,1] [,2]
## [1,]   -2.5  2
## [2,]    1.5 -1

test2<-makeCacheMatrix()    ## trying with a different, larger matrix
test2$set(matrix(c(1,5,3,7,8,13,14,6,4),3)) ## defining the new matrix, and setting it to "test2"



# typing: cacheSolve(test2)
# returns: 
##            [,1]        [,2]        [,3]
## [1,] -0.089494163  0.29961089 -0.13618677
## [2,] -0.003891051 -0.07392996  0.12451362
## [3,]  0.079766537  0.01556420 -0.05252918

# retyping: cacheSolve(test2)
# returns:
## getting cached data
##             [,1]        [,2]        [,3]
## [1,] -0.089494163  0.29961089 -0.13618677
## [2,] -0.003891051 -0.07392996  0.12451362
## [3,]  0.079766537  0.01556420 -0.05252918



## Acknowledgements
# Google and "http://stackoverflow.com/questions/6572119/r-solvesystem-is-exactly-singular" was used to help determine why I got the following Error-message:
# "Error in solve.default(matrix, ...) : Lapack routine dgesv: system is exactly singular: U[3,3] = 0"
# I found out that the test-matrix I had created was a singular matrix and therefore could not be inverted. By changing the values in the matrix, the problem was solved.



