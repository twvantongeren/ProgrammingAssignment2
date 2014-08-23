## Put comments here that give an overall description of what your
## functions do
#
# makeCacheMatrix() creates a list, 
# in which both the data (in this case the matrix elements of the matrix)
# and functions to acces the data are stored.
# The list can be stored in a variable 
# in order to store the data+functions in memory.
# This could be done with cachedMatrix <- makeCacheMatrix(testMatrix)
# where testMatrix is an ordinary (numeric) matrix.
#
# The cacheSolve() function takes a matrix 
# and checks wether an inverted matrix is already present in the list
# and if it is the inverted matrix is returned. 
# If it is not solved it solves the matrix and writes the inverted matrix 
# in the "matInvert" of the matrix 
# and after that it returns the inverted matrix.


## Write a short comment describing this function
# This function outputs a list which holds actually 2 matrices. 
# One is used as a normal matrix (matOrig), the other (matInvert) will hold the
# inverse of the normal matrix.
# This function takes a normal (numerical) matrix, and returns
# a list in which it places a copy of the numerical matrix and
# initializes the inverse, and it also puts 4 functions in the list
# to get and set both matrices.
makeCacheMatrix <- function(matOrig = matrix())
{

    # Initialize the inverted matrix
    matInvert <- NULL

    # Set the elements of the original matrix
    setOrig <- function(matInput)
    {
        # copy the matrix which is given as an argument to the
        # makeCacheMatrix() function into the internal matrix.
        # because matOrig is not in the frame of the setOrig() function
        # but the (parent) makeCache Matrix we assign with "<<-"
        matOrig <<- matInput 
       
    }

    # Return the elements of the original matrix
    getOrig <- function() 
    {
        return(matOrig)
    }

    # This function allows access to the matInvert matrix
    # inside this function.
    setInvert <- function(matInput = matrix())
    {
        # this writes the variable matInput to matInvert
        # however matInvert resides in the parent environment to
        # the function setInvert so we use this special assignment
        matInvert <<- matInput 
    }

    # Return the elements of the inverted matrix
    getInvert <- function()
    {
        return(matInvert)
    }

    # Without this the functions are not accessible.
    list(setOrig = setOrig, getOrig = getOrig, 
         setInvert = setInvert, getInvert = getInvert)
    
}


## Write a short comment describing this function
# This function acts on the list created by
# the makeCacheMatrix() function.
# If the function is applied to a "cacheMatrix" object
# it will determine if there is an inverse matrix 
# by looking in the object to see
# if there is an inverted matrix stored.
# If it does not exist it will invert the matrix and
# store it to the cacheMatrix object, and after this it
# will return the inverted matrix.
# The matrix I am currently evaluating is named matEval
cacheSolve <- function(matEval, ...) 
{

    ## Return a matrix that is the inverse of 'matEval'
    # load the inverted matrix from the matrix under evaluation
    matInvertLoaded <- matEval$getInvert()

    # However if this is the first time the inverse of this matrix
    # is looked up the "matInvert" part of the object will still be NULL
    if(!is.null(matInvertLoaded))
    {
        print("retrieved from cache:")
        return(matInvertLoaded)
    } 
    
    # One only gets on this place if the matrix was not set.
    # This is because return(matInvertLoaded) will end this
    # function if it was set.
    
    # This means we have to calculate the inverse of the matrix.
    # For this we need the original matrix
    matOrigForCalc <- matEval$getOrig()

    # Invert it and put it in a temporal variable
    matInvertCalc <- solve(matOrigForCalc)
   
    # write the inverted matrix to the matrix 
    # which we are currently evaluating
    matEval$setInvert(matInvertCalc)
    
    # return the inverted matrix to the parent frame
    return(matInvertCalc)
    
}

