#honestly not sure what is going on here. It looks like somone
# forked the assignment, did the work, and then merged it back in.
# like seriously, there is nothing to do here.  both functions already
# do the work of the assignment.
# maybe if I change some variable names it iwll look like my work...
makeCacheMatrix <- function(z = matrix()) {
    mat <- NULL
    set <- function(holder) {
      z <<- holder
      mat <<- NULL
    }
    get <- function() z
    setinverse <- function(solve) mat <<- solve
    getinverse <- function() mat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse
    )
}


cacheSolve <- function(w, ...) {
    ## Return a matrix that is the inverse of 'w'
	mat <- w$getinverse()
    if(!is.null(mat)) {
      message("retrieving the cached inverse!!!")
      return(mat)
    }
    data <- w$get()
    mat <- solve(data, ...)
    w$setinverse(mat)
    mat
}

#Success! I checked, and same results returned!!
#    > a<-makeCacheMatrix(myMatrix)
#    > a$get()
#          [,1]  [,2]
#    [1,]  1.00 -0.25
#    [2,] -0.25  1.00
#
#    > cacheSolve(a)
#              [,1]      [,2]
#    [1,] 1.0666667 0.2666667
#    [2,] 0.2666667 1.0666667
#
#    > cacheSolve(a)
#    retrieving the cached inverse!!!
#              [,1]      [,2]
#    [1,] 1.0666667 0.2666667
#    [2,] 0.2666667 1.0666667
#
# so really, not sure what there is to do.