## cachematrix.R - Caching the Inverse of a Matrix.

## 1. makeCacheMatrix: creates a "matrix" object that
##    can cache its inverse.
## 2. cacheSolve: This function makes the inverse of "matrix"
##    returned by makeCacheMatrix 

## function  "makeCacheMatrix" do
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(i) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The following function "cacheSolve" brings the inverse of  "matrix" 
## 1- checks to see if the inverse of the matrix
## has already been calculated. 
## 2- If so, it gets the inverse from the cache and skips the
## computation. 
## 3 - Otherwise, it calculates the inverse of the data and sets the inverse
## matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...)
{
  m <- x$getInverse()
  if(!is.null(m))
  {
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}