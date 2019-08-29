
# function to save an inverse of an invertible matrix

makeCasheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  } # the "special" vector initialised
  
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i # the rest of the functions called using Lexical scoping
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) # final output as list of functions
}
}


# function to call the cached inverse of a matrix or
# perform the inverse operation in case there is no
# inverse stored in the cached memory

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i) # cached inverse returned IF present
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i) # newly calculated inverse returned
  i
}
}
