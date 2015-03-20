# cachedMatrix.R

# Writing the function cacheSolve and having 2 functions for this behaviour feels a bit overdone.
#
# so I skipped cacheSolve


# makeCacheMatrix
# returns a list of 3 functions to set (cache) & get a matrix and to get its (cached) inverse
makeCacheMatrix <- function(matrix) {
        inverse <- NULL  # the 'cached' inverse of the matrix
        
        # set the matrix, changing the matrix nullifies the inverse
        setMatrix <- function(myMatrix) {
                matrix <<- myMatrix
                inverse <<- NULL
        }
        
        # get the matrix
        getMatrix <- function() matrix
        
        # get the inverse, return the cached value if it's there
        # else calculate, cache and return the inverse
        getInverse <- function() {
                if (!is.null(inverse)) {
                        message("returning the cached inverse")
                        return(inverse)
                }
                inverse <<- solve(matrix)             
        }
        
        # return a list of above functions
        list(set=setMatrix, get=getMatrix, inverse=getInverse) 
}


# test driver voor matrix/inverse caching list (micl)
test_micl <- function() {
        micl <- makeCacheMatrix(matrix(runif(25),5,5))
        print(micl$get())
        
        print(micl$inverse())

        # should result in a "returning the cached inverse" message
        inverse <- micl$inverse()
        
        # change the cached matrix
        micl$set(matrix(runif(16),4,4))
        print(micl$get())
        
        # no pop up when getting the inverse
        print(micl$inverse())                
}

test_micl()