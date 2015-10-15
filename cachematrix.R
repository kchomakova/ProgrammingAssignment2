## The following two functions are supposed to store the values of an invertible matrix and its inverse matrix 
## and calculate and cache the inverse matrix in case the original invertible matrix has changed.

## The function makeCacheMatrix takes as a single argument an invertible matrix and stores a list of four other 
## functions that are supposed to: reset the value of the matrix in the main function; get (return) the value of
## the main function's argument; cache the value of the inverse matrix and return the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        InvMtx <- NULL
        
        set <- function(y) {
                x <<- y
                InvMtx <<- NULL 
        }
        
        get <- function() x
        
        setInvMtx <- function(k) InvMtx <<- k 
        
        getInvMtx <- function() InvMtx
        
        list(set = set, get = get, setInvMtx = setInvMtx, getInvMtx = getInvMtx)
}


## The function cacheSolve takes as an argument an object where makeCacheMatrix is stored and then does the following:
## returns the value for the inverse matrix stored by makeCacheMatrix$setInvMtx(k); 
## he value is NULL, the inverse mateix is calculated, stored and returned.

cacheSolve <- function(x, ...) {
        InvMtx <- x$getInvMtx()
        
        if(!is.null(InvMtx)) {
                message("getting cashed data")
                return(InvMtx)
        }
        
        else {
                matrix <- x$get()
                InvMtx <- solve(matrix,...)
                x$setInvMtx(InvMtx)
                return(InvMtx)
        }
}

#Practical example
a <- makeCacheMatrix(matrix(c(rep(1,3),3,4,3,3,3,4),3,3)) #An invertible matrix is stored

a$get() #The stored matrix is returned
#      [,1] [,2] [,3]
# [1,]    1    3    3
# [2,]    1    4    3
# [3,]    1    3    4

a$getInvMtx() #There is no value for the inverse matrix cached 

a$setInvMtx(matrix(c(7,-1,-1,-3,1,0,-3,0,1),3,3)) #Caches the value for the inverse matrix. The value is stored in the
                                                  #main function's (makeCacheMatrix) environment

a$getInvMtx()   #Returns the cached inverse matrix
#      [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1

a$set(matrix(c(1,-5,0,0,1,0,0,0,1),3,3)) #Changes the value of the original matrix. The updated value is stored in the main funcion.
                                         #At the same time the value for the inverse matrix in the main function is reset  
                                         #back again to NULL since it is no longer correct.

a$getInvMtx() #NULL

cacheSolve(a) #Checks if the inverse matrix is cached and since it is not (row 70), calculates the inverse matrix and stores it 
              #in makeCacheMatrix's environment
#      [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    5    1    0
# [3,]    0    0    1

cacheSolve(a)          #Since the inverse matrix is already stored its value is retrieved and returned without calculations.
# getting cashed data
#      [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    5    1    0
# [3,]    0    0    1