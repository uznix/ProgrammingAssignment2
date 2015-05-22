## Matrix inversion functions.

## Initiates functions to invert and cashe matrix.
makeCacheMatrix <- function(x = matrix()) {
  solved_mtrx <- NULL
  set <- function(y) {
    x <<- y
    solved_mtrx <<- NULL
  }
  get <- function() x
  set_solv_mtrx <- function(solve) solved_mtrx <<- solve
  get_solv_mtrx <- function() solved_mtrx
  list(set = set, get = get,
       set_solv_mtrx = set_solv_mtrx,
       get_solv_mtrx = get_solv_mtrx)
}


## The function returns inverse matrix.
cacheSolve <- function(x, ...) {
  solved_mtrx <- x$get_solv_mtrx()
  ## Check if the inverse matrix already cashed
  if(!is.null(solved_mtrx)) {
    message("getting cached data")
    return(solved_mtrx) ## If yes get inverse matrix from cashe
  }
  ## If not solves the input matrix
  data <- x$get()
  solved_mtrx <- solve(data, ...)
  x$set_solv_mtrx(solved_mtrx)
  solved_mtrx ## Return a matrix that is the inverse of 'x'
}
