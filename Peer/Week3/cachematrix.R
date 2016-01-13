## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The first function, makeVector creates a special "matrix", 
#which is really a list containing functions to
# set the value of the matrix
# get the value of the matrix  
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <-function(y){
                x<<-y
                inv <- NULL
        }
        get<-function()x
        setInv <- function(inv) inv <<-inverse
        getInv <- function() inv
        list(set = set, get =get, setInv=setInv, getInv=getInv)
}




## Write a short comment describing this function

#The following function calculates the inverse of the special "Matrix" created with the above function.
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the
#inverse in the cache via the setInv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <-solve(data,...)
        x$setInv(inv)
        inv # return
}



# makeVector <- function(x = numeric()) {
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
# }
# 
# 
# 
# cachemean <- function(x, ...) {
#         m <- x$getmean()
#         if(!is.null(m)) {
#                 message("getting cached data")
#                 return(m)
#         }
#         data <- x$get()
#         m <- mean(data, ...)
#         x$setmean(m)
#         m
# }
