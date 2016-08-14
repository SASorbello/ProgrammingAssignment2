## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        inv = NULL
        set = function(y) {

                x <<- y
                inv <<- NULL #store matrix in cache
        }
        get = function() x #get matrix
        setinv = function(inverse) inv <<- inverse  #set inverse matrix
        getinv = function() inv #get inverse matrix
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
        inv = x$getinv() #query the x matrix's cache
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data") # sent message indicating this is just cache
                return(inv) # return the cache
        }
        
        # otherwise, 
        mat.data = x$get()
        inv = solve(mat.data, ...)  # calculate the inverse of the matrix
        
        x$setinv(inv) # store the inverse matrix in cache
        
        return(inv)
}
