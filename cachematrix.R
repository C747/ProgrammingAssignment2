## Below two functions are used to create a special object and store the matrix and cache's its inverse 

## The first function, makeCacheMatric creates a special "matric", which is really a list containing a function to
##set and get the value of the matrix, set and get the value of the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
    xi <- NULL
    set <- function(y) {
      x <<- y
      xi <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xi <<- inv
    getinv <- function() xi
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The following function, cacheSolve first checks if the inverse of matrix is already calculated. If so,it return the 
##calculated inverse from the cache. Otherwise,it calculates the cache and set the inverse in the cache via setinv function 

cacheSolve <- function(x, ...) {
  xi=x$getinv()
  if(!is.null(xi)){
    message("getting cached data")
    return (xi)}
  mat=x$get()
  xi=solve(mat)
  x$setinv(xi)
  xi        ## Return a matrix that is the inverse of 'x'
}
