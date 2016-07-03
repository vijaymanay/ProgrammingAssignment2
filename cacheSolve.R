## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  ## check to see if inverse has already been calculated
  if (!is.null(inv)){
    ## get it from the cache. 
    message("getting cached data")
    return(inv)
  }
  
  ## else, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  ## sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}