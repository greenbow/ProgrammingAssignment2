makeCacheMatrix <- function(x = matrix()) {
x.inv <- NULL
set <- function(y) {
x <<- y
x.inv <<- NULL
}
get <- function() x
set.inv <- function(solve) x.inv <<- solve
get.inv <- function() x.inv
list(set = set, get = get,
set.inv = set.inv,
get.inv = get.inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
x.inv <- x$get.inv()
if(!is.null(x.inv)) {
message("getting cached matrix inverse")
return(x.inv)
}
data <- x$get()
x.inv <- solve(data, ...)
x$set.inv(x.inv)
x.inv
}
