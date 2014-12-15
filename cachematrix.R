## makeCacheMatrix function is used to create an matrix
## and to store its inverse. 
## set function is used to set the value of matrix
## get function is used to print the matrix
## setinv function is used to assign matrix i the value of inverse of input matrix
## getinv function is used to print the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
set<-function(y)
{
x<<-y
i<<-NULL
}
get<-function()
{
x
}
setinv<-function(inv)
{
i<<-inv
}
getinv<-function()
{
i
}
list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve is used to calculate the inverse of matrix using the function solve()
## it first calls the getinv function of makeCacheMatrix
## if it already contains the inverse of given matrix then it prints it
## if not then it calculates the inverse and gives the value to setinv

cacheSolve <- function(x, ...) {
        
	m<-x$getinv()
	if(!is.null(m))
	{
	message("getting cached data")
	return(m)
	}
        else
	{
	data<-x$get()
	m<-solve(data)
	x$setinv(m)
	{
	m
	}
	}
}
