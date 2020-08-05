##creates a matrix object capable of cache its inverse
##first we set the function
makeCacheMatrix <- function(x = matrix()) {
        ##initial parameters
        n1 <- NULL
        set <- function(m1){
                x <<- m1
                n1 <<- NULL
        }
        ##what to do with them
        get <- function() x
        ##working the function
        setinverse <- function(solve) n1<<-solve
        getinverse <- function() n1
        list(set=set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}
##next we creat the function to compute the special matrix created 
##by makeCacheMatrix Above
cacheSolve <- function(x, ...) {
        ##starting point
        n1<-x$getinverse()
        ##processing message
        if (!is.null(n1)){
                message("getting cached data")
                return (n1)
        }
        ##solving
        data <- x$get()
        n1 <- solve(data)
        x$setinverse(n1)
        n1
}