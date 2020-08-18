## This assignment contains two types of functions: 
## `makeCacheMatrix` and `cacheSolve`


## Example input with a matrix 4x4: x <- matrix(rnorm(64),4,4)
## Run the first function and store it in any names: check_mat <- makeCacheMatrix(x)
## Obtain the inverse matrix using: cacheSolve(check_mat)


## `makeCacheMatrix`: This function creates a special "matrix" object
##                  that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {   #initialize x
  Mx_inv <- NULL   #initialize Mx_inv
  set <- function(y) {   #define `set` function
    x <<- y
    Mx_inv <<- NULL
  }
  get <- function() x   #define `get` function for the vector x
  setMx_inv <- function(solve) Mx_inv <<- solve   #cache value for the inverse matrix
  getMx_inv <- function() Mx_inv   #get the cached value returning the inverse matrix
  list(set = set, get = get,   #create a list for the four functions
       setMx_inv = setMx_inv,
       getMx_inv = getMx_inv)
}


## `cacheSolve`: This function computes the inverse of the special
##            "matrix" returned by `makeCacheMatrix` above. If the inverse has
##             already been calculated (and the matrix has not changed), then
##            `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {   
  Mx_inv <- x$getMx_inv()   #get the result from `makeCacheMatrix`
  if(!is.null(Mx_inv)) {   #check if the result is null
    message("getting cached data")
    return(Mx_inv)
  }
  #if `!is.null(Mx_inv)` is FALSE then
  Mx <- x$get()   #get the vector x
  Mx_inv <- solve(Mx, ...)   #calculate the value using solve()
  x$setMx_inv(Mx_inv)   #set the inverse of the input matrix
  Mx_inv   #return the inverse matrix
  #end
}
