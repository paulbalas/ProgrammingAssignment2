makeCacheMatrix <- function(x = matrix()) {
 
  ## This function creates a special "matrix" object that can cache its inverse.
  
  ## initialize the matrix
  m<-NULL
  
  ## create function 'set' to assign input 'y' to 'x' where 'y' is passed as a parm, and to initialize 'm' with NULL
  set<-function(y){
    ## assign 
    x<<-y
    m<<-NULL
  }
  
  ## set get assigned function () x
  get<-function() x
  ## define setmatrix as function(solve) m which is assigned solve
  setmatrix<-function(solve) m <<- solve
  ## define getmatrix as function(solve) m 
  getmatrix<-function() m
  ## Functions to construct, coerce and check for both kinds of R lists.
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then cacheSolve should retrieve the inverse from the cache.
  
  m<-x$getmatrix()
  if(!is.null(matrx)){
    message("getting cached data")
    return(m)
  }
  ## assign to matrix from get function of x
  matrix<-x$get()
  
  ##  if 'x' is a square invertible matrix, then solve(X) returns its inverse.
  ## This generic function solves the equation a %*% x = b for x, where b can be either a vector or a matrix.
 
  ## a square numeric or complex matrix containing the coefficients of the linear system. Logical matrices are coerced to numeric.
  ## b	a numeric or complex vector or matrix giving the right-hand side(s) of the linear system. If missing, b is taken to be an 
  ##     identity matrix and solve will return the inverse of a.
  ## tol  the tolerance for detecting linear dependencies in the columns of a. The default is .Machine$double.eps. Not currently used with complex matrices a.
  ## ... further arguments passed to or from other methods
  matrx<-solve(matrix, ...)
  ## call setmatrix defined in other function passing m, also defined in other function
  x$setmatrix(m)
  m
}
