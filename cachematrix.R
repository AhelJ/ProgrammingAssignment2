## For an input matrix, this pair of functions calculates the inverse matrix
## or if the calculation was already made, returns the pre-calculated value.


## Define output variable with a unique diagnostic value. Define sub-functions
## that allow inputting a new value and resetting the diagnostic variable (set), 
## recovery of the current calculated solution (getinv), and setting the solution value (setinv). 
## Importantly, the solution value in setinv replaces the initial diagnostic value 
## in the parent function. Store these functions into a list, so they can be 
## called with subsetting ($). This list is the output of makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- matrix(NA)
        set <- function(y) {
                x <<- y
                i <<- matrix(NA)
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv= setinv,
             getinv = getinv)
        
}

## Call the pre-defined getinv function, and store the value in i. Check if
## the value of i equals the diagnostic empty value. If it does not, then
## it must have already been calculated and the value of i is printed, along with
## the message. Otherwise, data from the input are recovered using the pre-defined
## get() funtion, and the value of i (which was diagnostic) is replaced with 
## a calculated matrix inverse. x$setinv(i) replaces i in the parent environment
## with the new calculation so that it can be checked for the next calculation.
## Finally, the calculated matrix inverse is returned.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(is.na(i[1,1])==FALSE) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
