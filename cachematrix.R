## Functions work with a memoized matrix inversion

## Definition of memoized matrix inversion
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Memoized matrix solve().  Uses cached matrix inversion if exists.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("INFO: Returning cached inverted matrix.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## Tests
message("Testing memoized matrix\n=============")
message("Create random memoized matrix ...")
m <- matrix(ceiling(sapply(runif(9), function(x) x*10)), nrow=3)
cm <- makeCacheMatrix(m)
print(cm$get())
message("Solve memoized matrix ...")
cs <- cacheSolve(cm)
print(cs)
message("Re-run solve ...")
cs <- cacheSolve(cm)
print(cs)
