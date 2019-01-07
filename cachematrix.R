## This function will gets a "matrix" as an input, set the value of the matrix,
## get the value of the matrix, set the inverse and get the inverse Matrix.

## <<- operator is used to assign a value to an object in an environment that
## is different from the current environment

#takes the matrix as an input
makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        
        #set the value of the Matrix
        setValMatrix <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        #get the value of the Matrix
        getValMatrix <- function() x
        
        #set the value of the Invertible Matrix
        setInvMatrix <- function(inverse) inverseMatrix <<- inverse
        
        #get the value of the Invertible Matrix
        getInvMatrix <- function() inverseMatrix
        
        list(setValMatrix = setValMatrix, getValMatrix = getValMatrix,
             setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## rom the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInvMatrix()
        if(!is.null(inverseMatrix)) {    #if inverse matrix is not null
                message("Getting cached Invertible Matrix") #print message
                return(inverseMatrix) #return the invertible matrix
        }
        
        #if value of invertible matrix is NULL, then
        matrixData <- x$getValMatrix() #get the matrix data
        inverseMatrix <- solve(matrixData, ...) #use solve function to inverse
        x$setInvMatrix(inverseMatrix) #set the invertible matrix
        return(inverseMatrix)   #return the invertible matrix
}


#f76ff9531f70d30df26221ca5ad84a4826fc5bc0