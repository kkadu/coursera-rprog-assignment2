## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix assigns the cache for the inverse of the matrix and stores it 
makeCacheMatrix <- function(x = matrix()) {
		## Set a null value for the cache
		m <- NULL
		## Function to create and assign value for the matrix in this env
		set <- function(y = matrix()) {
				x <<- y
				m <<- NULL
		}
		## Retrieve Matrix
		get <- function() x
		## Invert the matrix and store in cache variable
		setinv <- function(solve) m <<- solve
		## Retrieve the invert from cache
		getinv <- function() m
		## Functions created in the environment
		list(set = set, get = get,
			setinv = setinv,
			getinv = getinv)
}


## Write a short comment describing this function

## Calculates the inverse of the matrix if not already stored in cache by the above function else calculates it run-time
cacheSolve <- function(x, ...) {
        ## creates a matrix if it was the same in previous function for testing that the matrix has not changed
		data <- x$get()
		## print(data)
		## getting the stored value for above function O/P i.e. inverse from cache
		m <- x$getinv()
		## if inverse exists then matrix has possibly not changed and if identical confirms certainty and checks if the matrix is invertible, matrix product should give identity matrix
        if(!is.null(m) && identical(data, x)) == 0 && sum(m %*% data - diag(nrow(data))) == 0) {
                message("Getting data from cache")
                return(m)
        }
		## if matrix changed or inverse not retrieved from cache, compute inverse, set the cache run-time and return the inverse
        m <- solve(data)
        x$setinv(m)
        m
}
## Define c <- matrix(c(), x,y)
## c1 <- makeCacheMatrix(c)
## cacheSolve(c1) ...(2)
## 1st run to store in cache and 2nd to retrieve from cache
