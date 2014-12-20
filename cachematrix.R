##README: This cachematrix.R script illustrates using namespace variable environments to cache results of 
##a calculation, specifically matrix inversion.
##The script defines 3 functions:
##(1) makeCacheMatrix
##(2) cacheSolve
##(3) run

## Function: makeCacheMatrix
## Arguments: a matrix object
## Description: This special matrix object creates internal function objects to set/get the value of an internally
## defined variable cashed_mInv, which enables the object to cashe the inverse of its input argument matrix
makeCacheMatrix <- function(x = matrix()) 
{
     cashed_mInv <- NULL
     
     setMatrix <- function(cv)
     {
          x <<- cv
          cashed_mInv <<- NULL
     }
     getMatrix <- function() { return(x) }
     setInverse <- function(mInv) { cashed_mInv <<- mInv }
     getInverse <- function() { return(cashed_mInv) }
     
     return(list(setInverse = setInverse, getInverse = getInverse, setMatrix = setMatrix, getMatrix = getMatrix))
}


## Function: cacheSolve
## Arguments: a makeCasheMatrix object
## Description: returns the inverse matrix of the matrix value defined in the input argument object
## the first time the casheSolve function is passed a given argument object, it will cause the object to 
## calculate the matrix inverse for the first time; the second time the function is called with the same
## argument object, it will return the cached value of the matrix inverse.
cacheSolve <- function(x) 
{
     ## Return a matrix that is the inverse of 'x'
     mInv <- x$getInverse()
     if (!is.null(mInv))
     {
          message("Using cashed value.")
     }
     else
     {
          message("Cashed value not found.  Calculating inverse.")
          m <- x$getMatrix()
          mInv <- solve(m)
          x$setInverse(mInv)
     }
     return(mInv)
     
}

## Function: run
## Arguments: matrix size
## Returns: null
## Description: a function that tests the cache functions above
##
## Sample output:
##run(999)
##Cashed value not found.  Calculating inverse.
##user  system elapsed 
##1.16    0.00    1.15 
##Using cashed value.
##user  system elapsed 
##0       0       0 
##[1] 999
##
##Test passes:
##(1) first evaluation takes 1.16s, second evaluation takes less than 1ms (0)
##(2) inverse correctly calculated as sum of inverse matrix %*% input matrix equals input size argument
run <- function(s)
{
     
     if (s > 1000) { return(print("Size limited to 1000 or smaller")) }  ## Don't want to eat up my memory...
     
     mat <- matrix(data = rexp(s^2, rate=10), nrow=s,ncol=s)  ## Create a dummy matrix based on input
     mcm <- makeCacheMatrix(mat) ## Create our special cache matrix object
     
     p <- proc.time()

     mInv <- cacheSolve(mcm) ## Cashed value should not exist, so calculate.  Note system time
     print(proc.time() - p)
     
     p <- proc.time()
     mInv <- cacheSolve(mcm)
     print(proc.time() - p)  ## If cashed value used, time is much, much shorter
     
     sum(mat %*% mInv)    ## Calculate the matrix multiplaction of input matrix and its inverse.  Sum should equal matrix size    
}