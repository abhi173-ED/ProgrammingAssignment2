##These two functions are intended to calculate the inverse of a matrix
##The first function, makeCachematrix, does the following four things:
##Sets the value of the matrix
##Gets the value of the matrix
##Sets the inverse of the matrix 
##Gets the value of the inverse
##Result is a special matrix from makeCachematrix 

makeCacheMatrix <- function(x = matrix()) {
        inv_1 <- NULL
        set <- function(y) {
                x <<- y
                inv_1 <<- NULL
        }
get <- function()x
setInverse <- function(inverse) inv_1 <<- inverse
getInverse <- function() inv_1
list (set = set,get = get,
      setInverse = setInverse,getInverse = getInverse)
}

##The next function, cacheSolve, calculates the inverse of the matrix
##First checks if the inverse has already been computed
##If yes, it gets the result from cache and skips further computation
##It not, it calculates the inverse and sets it in cache

cacheSolve <- function(x, ...) {
        ##Return a matrix that is the inverse of 'x'
        inv_1<- x$getInverse()
        if(!is.null(inv_1)) {
                message ("getting cached data")
                return(inv_1)
        }
        data <- x$get()
        inv_1 <- solve(data,...)
        x$setInverse(inv_1)
        inv_1
}
