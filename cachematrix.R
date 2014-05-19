## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a special objet that stores a matrix and cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
m_inv<- NULL # the variable in which store the inverse
    set <- function(y) { ## set the value of the matrix
        x <<- y ## assign y to x
        m_inv <<- NULL ## assign NULL to m_inv
    }
    get <- function() x ## get the value of the matrix
    setinv <- function(solve) m_inv<<-solve(x) ## set the value of the matrix inverse
    getinv <- function() m_inv ## get the value of the matrix inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) # return the "special matrix"
}


## Write a short comment describing this function
## Calculates the inverse of the "matrix "special matrix" created with the 
## makeCacheMatrix. This function forst checks if the inverse has already been 
## calculated


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv<-x$getinv()
    if(!is.null(m_inv)) { ## Check if the inverse has already claculated
        message("getting cached data") # if the answer is yes, then shown the message
        return(m_inv) ## return the inverse in the cache
    } 
    mymatrix <-x$get() # mymatrix is the original matrix
    m_inv <- solve(mymatrix) ## calculated the inverse
    m_inv<- x$setinv(m_inv) ## set the inverse from the "special matrix"
    m_inv # return the inverse
        
}
