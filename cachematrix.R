##this set of 2 functions takes advantage of storage
##capabilities combined with retrieval
##rather than computation

##makeCacheMatrix - creates a special matrix object
##that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  In_verse <- NULL
  set <- function(y) {
    x <<- y
    In_verse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) In_verse <<- solve
  getinverse <- function() In_verse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve tries to get the inverse from cache
##if it does not succed it computes the inverse

cacheSolve <- function(x, ...) {
  In_verse <- x$getinverse()
  if(!is.null(In_verse)) {
    message("getting cached data")
    return(In_verse)
  }
  data <- x$get()
  In_verse <- solve(data, ...)
  x$setinverse(In_verse)
  In_verse
}
