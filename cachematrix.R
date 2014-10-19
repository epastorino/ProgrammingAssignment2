## makeCacheMatrix() and cacheSolve()
## These functions can be used to speed up programs that make use of inverse
## matrix computations by caching the value of already computed inverse
## matrices.

## makeCacheMatrix creates a special matrix, which is really a list containing
## functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	# Initialize the inverse variable
    i <- NULL

    # Define the set() function
    # We use the <<- operator here because x and i live in the parent
    # environment of set(). Otherwise, x and i would refer to free variables
    # of set() insted of the x and i variables from  makeCacheMatrix().
    set <- function(y) {
            x <<- y
            i <<- NULL
    }

    # Define the get() function. Returns the value of the matrix.
    get <- function() x

    # Define the setinverse() function.
    # We also use the <<- operator here beacuse we need to set inverse to i,
    # which lives in the parent environment of setinverse().
    setinverse <- function(inverse) i <<- inverse

    # Define the getinverse() function. Returns the stored value of the inverse
    getinverse <- function() i

    # Return the "special" matrix, containing the set(), get(),
    # setinverse() and getinverse() functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve retrieves the inverse of a matrix created with makeCacheMatrix.
## It first checks if the inverse has already been calculated. If so,
## it returns the cached value and skips the computation. Otherwise, it
## calculates the inverse via the setinverse function.

cacheSolve <- function(x, ...) {

	# First, get the stored value of the inverse of x:
    i <- x$getinverse()

    # If the inverse is not null, it means we've already computed the inverse,
    # so we just return the stored value.
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }

    # If the inverse value is NULL, it means we haven't computed it yet, so:
    # 1. We get the value of the matrix
    data <- x$get()
    # 2. We compute the inverse of the matrix using the solve funcion:
    i <- solve(data, ...)
    # 3. We cache the value of the inverse matrix
    x$setinverse(i)
    # 4. Finally, we return the inverse
    i
}
