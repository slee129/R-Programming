# R-Programming-Assignment-2
##makeCacheMatrix will create the initial matrix and put it in cache as well as call upon
##functions that will eventually invert it and store it in cache 
##cacheSolve will invert the initial matrix and store it in cache and then return those values stored


##this function will create the maxtrix and cache it
makeCacheMatrix <- function(x = matrix()) 
{
  ##making cache equal to NULL at first 
  cache <- NULL
  
  ##this will create the matrix in the working environment
  creatematrix <- function(y) 
  {
    x <<- y ##assigning a value to x from this different function in a different environment 
    
    cache <<- NULL ##assigning NULL to cache in this different environment
    
  }
  
 ##a functiom that retrieves maxtrix values   
 retrievematrix <- function()x
 
 ##a function that inverts the matrix and stores the new values in cache
 invertmatrix <- function(inverse) cache <<- inverse 

 ##a function that retrieves inverted matrix values from the cache
 retrieveinverts <- function() cache
 
 ##returns the previously written functions back to current environment 
 list(creatematrix = creatematrix, retrievematrix = retrievematrix, invertmatrix = invertmatrix,
      retrieveinverts = retrieveinverts)

}


##cachesolve will invert the matrix initially created in makCacheMatrix
##if the inverted matrix does not exist in cache, it is created in the working evironment and then stored in cache
cacheSolve <- function(x, ...) 
{
  ##if inverts already created...
  ##retrieve the inverse values of the matrix stored in cache
  ##if inverts not already create cache will equal to null
  cache <- x$retrieveinverts
  
  ##if cache is not equall to null
  if (!is.null(cache)) 
  {
    return(cache) ##return what is in the cache, assuming it is the inverted values 
    
  }
  
  ##if the if statement is skipped and the cache does equal to Null
  ##then need to create the matrix first 
  mymatrix <- x$retrievematrix()
  
  ##using the solve function to create the inverted matrix and put it in cache
  cache <- solve(mymatrix, ...)
  
  ##put the inverted matrix in cache
  x$invertmatrix(cache)
  
    
  ##return the final value in cache
  ##should be the inverted values or an error message if matrix not invertible
  return(cache)
  

}
