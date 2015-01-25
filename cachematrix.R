# This script contains two functions, "makeCacheMatrix" and "cacheSolve".
#"makeCacheMatrix" requires a square invertible matrix as a formal argument,
#and creates a four element list of four functions.
#"cacheSolve" requires the four element list of four functions as a formal argument and will return
#the inverse of matrix defined within "makeCacheMatrix".


#Creates a four element list where the elements are functions.  The user has the option
#to call the four functions manually.  The function defines a matrix variable m in the global
#environment to be used within the makeCacheMatrix functions and the cacheSolve function.  

#requires the formal agrument x as a square invertible matrix defined in the globabl env.
makeCacheMatrix <- function(x = matrix()) {

  #defines the variable m, in the globabl env., as NULL
  m <-NULL
  
  #define the function set within the "makeCacheMatrix" environment and updates 
  #the value of x to be the formal argument to the set function, y.  The function also
  #returns the value of m to NULL.  That is to say, if x changes, m becomes NULL.
  set <- function(y)
  {
    
    #search for the for the variable x in y's parent environment (glob. env.) and replace the value of x
    #with the value of.  Set m to NULL in the parent environment (glob. env.).
    #Whenever the matrix x is chanded through set, the inverse needs to be re-calculated.
    x <<- y
    m <<-NULL
  
  }
  
  #Return the value of x
  get <- function()x
  
  #Calculate the inverse of the square and invertible matrix x and requires
  #a formal argument of matrix x, which presumably square an invertible.
  setinverse <- function(x) 
    {
    
    #call R function solve() with x as the formal parameter to solve for x^-1 and set the value
    #of m in the parent environment to the value of inverse matrix of x
    m<<-solve(x)
    
  } 
  
  #return the value of m as defined in the parent environment
  getinverse <- function() m
  
  #create a list where the elements are name set, get, setinverse, and getinverse, and each element
  #stores the function of the same name to be called manually if wanted.
  #If "makeCacheMatrix" is assigned to a variable, this list is returned to the variable.
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}


#This function either returns a cached inverse matrix stored in the "makeCacheMatrix", or 
#calculates inverse of the base matrix stored in "makeCacheFunction", updates the value of the inverse
#in "makeCacheMatrix", and returns the value of the inverse.


#Requires the formal x as a vector storing the list of functions and variables defined in
#"makeCacheMatrix" environment.
cacheSolve <- function(x, ...) {
  
       
  #Calls the function "x$getinverse()" [x is the named list to which the "makeCacheMatrix" output
  #is assigned], which returns the value of m stored in the environment and then stores that to m.
  #Note: m is being called from and updated in the same parent environment.
  #"makeCacheMatrix"
  m <- x$getinverse()
  
  #if m is not NULL then return the value of m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #else if m is NULL
  
  #call function x$get() which will return the matrix, defined as either formal parament to or
  #the set() function in "makeCacheMatrix", stored the "makeCacheMatrix" environment
  #and store the returned value in the variable named data, defined in the parent environment.
  data <- x$get()
  
  #Call the R function solve to solve for the inverse of the value of variable data
  m <- solve(data)
  
  #Update the inverse matrix in the "makeCacheMatrix" by calling the setinverse function from
  #the function list, which also updates the value m in the environment of "makeCacheMatrix"
  x$setinverse(data)
  
  #return the value of m
  m
  
}
