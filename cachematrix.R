## This function stores a matrix and it's inverse to the global environment
## the commented out print commands were used to isolate R programming errors and
## to document the function being performed

makeCacheMatrix <- function(x = matrix()) {
  ## Create a blank matrix to hold the inverse of x
  m_inv <- NULL
  
  ## define the functions that can be performed on the matrix "x"
  
  ## Define a function that saves the matrix to the global environment
  set_m <- function(local_mat) {
      ## print (local_mat)
      ## print ("save local matrix to the global environment")
      assign("x",local_mat, envir = .GlobalEnv)
      ## print (x)
      ## print ("set matrix inverse to null and save it to the environment")
      assign("m_inv", NULL, envir = .GlobalEnv)
      ## print (m_inv)
  }
  
  ## Define a function that retrieves the matrix from the global environment
  get_m <- function() { 
    ## print ("getting the matrix")
    m <- get ("x", envir = .GlobalEnv)
    ## print (x)
    return (m)
  }
  
  ## create a function called "setinv" to solve the matrix "m" and 
  ## store it in the variable m_inv in the global environment
  setinv <- function(x) {
    m_inv <- solve(x)
    assign("m_inv", m_inv, envir = .GlobalEnv)
    ##print ("setting the inverse")
    ## print (m_inv)
    return(m_inv)
  }
  
  ## create a function called "getinv" to retrieve the inverse of  matrix "x"
  getinv <- function() { 
    if (exists("m_inv", envir =.GlobalEnv)) {
      m_inv <- get ("m_inv", envir = .GlobalEnv )
    } else {
      m_inv <-NULL
    }
  
    ## print ("This is the retrieved value")
    ## print(m_inv)
    return(m_inv)
  }
   
  ## print ("define the list of functions")
  list(set = set_m,
       get = get_m,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x) {
  ## create a local variable to store the matrix
  temp_mat <- makeCacheMatrix(x)
  ## print(temp_mat)
  
  ## Create a local variable to store the inverse
  ## print("Getting the value of the inverse")
  temp_inv <- temp_mat$getinv()
  ## print (temp_inv)
  
  ## Check to see if it is a NULL value
  if(is.null(temp_inv)) {
    ## print ("Solve for the inverse of the matrix, and store it in the variable 'data'")
    data <- temp_mat$get()
    ## print ("Printing the returned matrix")
    ## print (data)
    ## print ("save the temporary variable of the tempinv to the environment")
    temp_inv <- temp_mat$setinv(data)
  } else {
    message("Returning the cached data for the inverse")
    return(temp_mat$getinv())
  }
    
  ## print ("Return the answer")
  return(temp_inv)
}
