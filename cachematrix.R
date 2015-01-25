# This script contains two functions, "makeCacheMatrix" and "cacheSolve".
#"makeCacheMatrix" requires a square invertible matrix as a formal argument,
#and creates a list of four functions.
#"cacheSolve" requires the four element list as a formal argument and will return
#the inverse of the matrix defined within "makeCacheMatrix".


#Creates a four element list where the elements are functions.  The user has the option
#to call the four functions manually.  The function defines a matrix variable m in the global
#environment to be used within the makeCacheMatrix functions and the cacheSolve function.  

#requires the formal agrument x as a square invertible matrix defined in the globabl environment.
makeCacheMatrix <- function(x = matrix()) {

  #Defines the variable m, in the globabl env., as NULL
  m <-NULL
  
  #Define the function set() within the "makeCacheMatrix" environment and update 
  #the value of x to be the formal argument to the function set(), y.  The function also
  #returns the value of m to NULL.  That is to say, if x changes through set(), m becomes NULL.
  set <- function(y)
  {
    
    #Search for the for the variable x in y's parent environment (glob. env.)
    #and replace the value of x with the value of.  
    #Set m to NULL in the parent environment (glob. env.).
    #Whenever the matrix x is chanded through set, the inverse needs to be re-calculated.
    x <<- y
    m <<-NULL
  
  }
  
  #Return the value of x
  get <- function()x
  
  #Calculates the inverse of the formal argument matrix x, which is presumably square and invertible.
  setinverse <- function(x) 
    {
    
    #Call the function solve() with x as the formal argument to solve for inverse of x 
    #and set the value of m in the parent environment to the value of inverse matrix of x.
    m<<-solve(x)
    
  } 
  
  #Return the value of m as defined in the parent environment.
  getinverse <- function() m
  
  #Create a list where the elements are named set, get, setinverse, and getinverse, and each element
  #stores the function of the same name to be called manually if wanted.
  #If "makeCacheMatrix" is assigned to a variable, this list is returned to the variable.
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}


#"cacheSolve either returns a cached inverse matrix stored in the "makeCacheMatrix", or 
#calculates the inverse of the base matrix stored in "makeCacheFunction", 
#updates the value of the inverse in the formal argument's list variable, 
#and returns the inverse matrix.

#Requires the formal x as a vector storing the list of functions and variables defined in
#the "makeCacheMatrix" environment.
cacheSolve <- function(x, ...) {
  
       
  #Calls the function "x$getinverse()" [x is the named list to which the "makeCacheMatrix" output
  #is assigned], which returns the value of m stored in "makeCacheMatrix" environment 
  #and then stores that to m in the current environment.
  m <- x$getinverse()
  
  #if m is not NULL then return the value of m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #else if m is NULL:
  
  #Call function x$get(), which will return the matrix, defined as either formal argument to or by
  #the set() function in "makeCacheMatrix", stored in the "makeCacheMatrix" environment.
  #Then the result is stored in the variable named data, defined in the parent 
  #environment of "cacheSolve"
  data <- x$get()
  
  #Call the R function solve() to solve for the inverse of the value of variable data.
  m <- solve(data)
  
  #Update the inverse matrix in the "makeCacheMatrix" environment by calling
  #the setinverse() function from the function list, which also updates 
  #the value m in the environment of "makeCacheMatrix".
  x$setinverse(data)
  
  #return the value of m
  m
  
}
