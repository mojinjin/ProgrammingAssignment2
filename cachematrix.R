## Matrix inversion is usually a costly computation.It the contents of a matrix ## are not changing,it may make sense to cache the value of the inversion so that ## when we need it again,it can be looked up in the cache rather than ## recomputed.Here is a pair of functions that cache the inverse of a matrix.


## The first function,makeCacheMatrix,creats a special "matrix" object that can ## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setsolve <- function(solve) m <<- solve
            getsolve <- function() m
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)

}


## The secord function,cacheSolve,computes the inverse of the special "matrix" ## returned by makeCacheMatrix above.If the inverse has already been calculated ## (and the matrix has not changed),then this function should retrive the inverse ## from the cache.

cacheSolve <- function(x, ...) {
         m <- x$getsolve()
         if(!is.null(m)) {
               message("getting cached data")
               return(m)
         }
         data <- x$get()
         m <- solve(data,...)
         x$setsolve(m)
         m
}
