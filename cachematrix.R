## The following two functions work in conjuction to store a matrix and its inverse. 
## How to use: 
## 1) store your matrix with makeCacheMatrix
##      MyList <- makeCacheMatrix(MyMatrix)
## 2) calculate the inverse
##      cacheSolve(MyList)
## 3) if you need to calculate the inverse again, 
##      the function will pull the stored value instead of recalculating


## This function takes a matrix and creates a list of functions
## Note that this outputs a list, and not a matrix
## The list contains functions that can be called to get the matrices
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL #intialize an empty varible for the inverse of the matrix
    
    #The set function is used to overwrite the current matrix with a new one
    set <- function(y = matrix()) {
        x <<- y
        inverse <<- NULL #We assume the new matrix will have a different inverse, so we reset the inverse
    }
    #The get function is used to return the matrix
    #Note that 'makeCacheMatrix' is a list, so to get the matrix, you have to use the get function
    get <- function() {
        x
    }
    #The setinverse function takes an input and sets it as the inverse varible defined above
    #Note that setinverse can accept any matrix and does not check if the new matrix is the inverse of not
    setinverse <- function(input = matrix()){
        inverse <<- input
    } 
    #The get function is used to return the matrix that was assigned to 'inverse'
    #Note that 'makeCacheMatrix' is a list, so to get the inverse matrix, 
    #you have to use the getinverse function
    getinverse <- function() {
        inverse
    }
    #Finally, this is the list that is returned that contains all 4 functions
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function takes a list that was created from makeCacheMatrix(),
## checks to see if this list already has an inverse, 
## and returns the inverse of the matrix in this list.
cacheSolve <- function(x = list(), ...) {
    # Initializes a veriable, inverse, and set it to the inverse from the list
    inverse <- x$getinverse()
    # Checks to see if inverse is NULL
    if(!is.null(inverse)) {
        # If the inverse is not NULL, let the user know
        message("getting cached data")
        # returns the inverse matrix
        # Note that this is not a list
        return(inverse)
        # No futher calculations are done
    }
    # This part of the function will only execute if inverse is NULL
    # This is where the function will calculate the inverse of the matrix
    
    # First we need to get the matrix from the list
    data <- x$get()
    # Then we use solve() to calculate the inverse and assign it to our varible inverse
    inverse <- solve(data, ...)
    # Now that we have calculated the inverse, this stores the inverse back into the list
    # Note that since these two functions, makeCacheMatrix() and cacheSolve() are to work in conjuction,
    # the function setinverse() does not need to check if the input is actually an inverse
    x$setinverse(inverse)
    # Lastly, we return "inverse" which is the inverse of the matrix that was in the list
    inverse
}
