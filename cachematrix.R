## This set of functions builds an object to cache an inverse of a matrix.
## note: it is built from example given in directions for assignment 2
##  REALLY GREAT HELP HERE: 
##  https://github.com/DanieleP/PA2-clarifying_instructions
##  also, really good explaination (and examples) of matrix inverses here:
##  https://www.mathsisfun.com/algebra/matrix-inverse.html

## makeCacheMatrix is a "factory" function.
##  since it's purpose is to "build" an object with properties and sub-functions
##  makeCacheMatrix will take a matrix and make a new object with $get,$set, $setinverse,
##  and $getinverse functions
##
##  example usage: 
##  > myMatrix <- rbind(c(1, -1/4), c(-1/4, 1))
##  > a <- makeCacheMatrix(myMatrix)
##  > a$get
##        [,1]  [,2]
##  [1,]  1.00 -0.25
##  [2,] -0.25  1.00

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse
    )
}

## cacheSolve figures out if your CacheMatrix object has an inverse yet.
##  if it does, it returns that inverse
##  if it does not, it builds the inverse and stores it
##
##  example usage:
##  > myMatrix <- rbind(c(1, -1/4), c(-1/4, 1))
##  > a <- makeCacheMatrix(myMatrix)
##  > a$get
##        [,1]  [,2]
##  [1,]  1.00 -0.25
##  [2,] -0.25  1.00
##  > cacheSolve(a)
##            [,1]      [,2]
##  [1,] 1.0666667 0.2666667
##  [2,] 0.2666667 1.0666667

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached inverse")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
