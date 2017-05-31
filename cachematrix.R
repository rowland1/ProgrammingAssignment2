## These various functions will work with matrix, caching and alse create inverse of those items
# matrix cache
makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set.f <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get.f <- function() x
  set.inverse <- function(inverse) invrs <<- inverse
  get.inverse <- function() invrs
  out <- list(set=set.f, get=get.f, setinverse=set.inverse, getinverse=get.inverse)
  return(out)
}


# # test function 
# x = rbind(c(1, -1/4), c(-1/4, 1))
# m = makeCacheMatrix(x)
# m$get()



## Use the cached data to solve for the inverse function
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("obtain data cache.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
