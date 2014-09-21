## makeCacheMatrix contains 4 fns:set, get, setInverse and getInverse. 
## when makeCacheMatrix(x) is called it creates a new enviroment for those fns 
## to operate on the matrix x.

makeCacheMatrix <- function(x = matrix()) {
        ## before any calc, I is Null 
        I <- NULL
        ## set x to new value, Null cache I
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        ## return x to cmd line
        get <- function() x
        
        ## set I, cache
        setInverse <- function(Inverse) I <<- Inverse
        ## return I to cmd line
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## Return the cache value of I.  
## If I is currently Null compute the inverse using solve()

cacheSolve <- function(x, ...) {
        ## check current I
        I <- x$getInverse()
        ## if I is not Null return I
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        ## if I is Null, calculate I
        data <- x$get()
        ## calculate inverse of x
        I <- solve(data, ...)
        ## set new I, return
        x$setInverse(I)
        I
}

