## test line in
## This assignment has two functions that can be used to calculate the inverse of a matrix,
## cache it and then retrieve it. If the inverse is cached, it returns the cached value. 
## If not, it calculates it and caches it. The functions use "lexical scoping" to maintain
## the value of the inverse matrix and access it when needed.

## makeCacheMatrix(x)
##
## This function takes as an argument the matrix that we would to invert. Supplying an 
## argument is not mandatory, since the argument gets initialized as a function argument. 
## The function will define and then return a list of functions that can be used as an 
## argument by the "cacheSolve" function defined next. 
##
## There are two concepts that make this function work : 
## a) Lexical scoping, where the sub-functions (set(), get(), setInverse(), getInverse())
## defined within the parent function makeCacheMatrix() have access to variables defined
## in the parent function
## b) Returning a list of functions (or actually pointers to the functions) defined within
## the parent makeCacheMatrix(), prevents the memory consumed by makeCacheMatrix() to be 
## released, making the variable available after the parent function returns. 
##
## The following functions are defined :
## set(x) : takes a matrix as an argument and sets "x" in the parent environment
## get(x) : returns the matrix stored in the "x" variable in the parent environment
## setInverse(inverse_of_x) : sets the "x_inv" variable
## getInverse(inverse_of_x) : returns the value of "x_inv"
## 
## In esssence, this function creates an object in the form of a list of functions that 
## can set and get two variables : "x" and "x_inv" that given lexical scoping, will not 
## be cleared once the parent function retuns. Hence, the values will be cached !!
##
## Now, how do we populate the "x_inv" value ?  That is where "cacheSolve()" comes in !!
##

makeCacheMatrix <- function(x = matrix()) {

        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setInverse <- function(x_inverse) x_inv <<- x_inverse
        getInverse <- function() x_inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

## cacheSolve(myCacheList)
##
## This function takes as an argument a list that was created by the makeCacheMatrix() 
## function. It uses the functions in that list to set and get the matrix and the inverse of 
## matrix "x" which was used to call the makeCacheMatrix() function. If the inverse of exist 
## exists, it simply retrieves from cache and returns it, by using myCacheList$getInverse(). 
## If not, it calculates it and then stores it in cache by using myCacheList$getInverse()
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inverse <- x$getInverse()
        if(!is.null(x_inverse)) {
                message("getting cached data")
                return(x_inverse)
        }
        data <- x$get()
        x_inverse <- solve(data)
        x$setInverse(x_inverse)
        x_inverse
}

