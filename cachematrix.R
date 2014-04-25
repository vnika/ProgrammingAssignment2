##  creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
       x_inv <- NULL
       set <- function(y) {
              x <<- y
              x_inv <<- NULL
       }
       get <- function() x
       setinverse <- function(inverse) x_inv <<- inverse
       getinverse <- function() x_inv
       list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##  computes the inverse of the special "matrix" returned by `makeCacheMatrix` 
##  If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
       x_inv <- x$getinverse()
       if(!is.null(x_inv)){  # inverse matrix is already exists
              return(x_inv)
       }
       y <- x$get()
       x_inv <- solve(y)
       x$setinverse(x_inv)
       x_inv
}

example <- function(n){
       x = matrix(rnorm(n*n), ncol = n)
       if(abs(det(x)) < 0.1){
              dd = 0.1/n
              for(i in 1:n) x[i,i] = x[i,i] + dd
       } 
       x_cashe = makeCacheMatrix(x)
       x_inv = cacheSolve(x_cashe)
       sum(diag(x%*%x_inv))
}