## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL #initiating m to be null
        set<-function(y){
                x<<-y #taking in a matrix y and placing it in x
                m<<-NULL #making m NULL once this function is run
        }
        get<-function() x #taking the matrix x and putting into the object get
        invmatrix<-function(solve) m<<- solve # putting solve into invmatrix
        getmatrix<-function() m #putting in the NULL/matrix of m into getmatrix
        list(set=set, get=get, #outputting all of the objects in makeCacheMatrix
             invmatrix=invmatrix,
             getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix() #reading the object 'getmatrix' inside x to put in m
        if(!is.null(m)){ #testing if m has a matrix or null. if so return the matrix
                message("getting cached data")
                return(m)
        }
        matrix<-x$get() #when m is null go 'get' the created matrix
        m<-solve(matrix, ...) #find the inverse of the matrix
        x$invmatrix(m) # put the inverted matrix into the object invmatrix
        m #return the inverted matrix
}
        
        
