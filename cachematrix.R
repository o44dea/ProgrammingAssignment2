# Caching the inverse of a matrix adds computation benefit as it elimates need
# for repeated computations.
# The functions cache the inverse of a matrix.

# makeCacheMatrix function creates a special matrix object that can cache its inverse.
# The << operator assigns a value to an object in an environment different from the current
#environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set<- function(y) {
                x<<- y
                m<<- NULL
        }
        get<- function() x
        setinverse<- function(inverse) m <<- inverse
        getinverse<- function() m
        list (set=set, get=get, setinverse=setinverse, getinverse=getinverse) 

}


## cachSolve calculates the inverse of the squared matrix, above makeCacheMatrix.
## If the inverse has already been calculated and matrix not changed, the cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x , ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse ()
        if (!is.null(m)){
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
## Test - makeCacheMatrix - expected result - make object to cache inverse
 #x = cbind (c(8, 24), c(24,8)
 #          m = makeCacheMatrix(x)
#           m$get()
           # Result
 #          [,1] [,2]
 #       [1,] 8    24
 #       [2,] 24    8
                  
                             

