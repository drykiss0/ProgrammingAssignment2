###################################################################
## Following two functions implement caching of a matrix inverse
## in order to avoid redundant inverse computation
###################################################################
 
# Creates a list of functions, used to get/set matrix and its inverse.
# Represents a matrix and its inverse cache.
# Args:
#     mtx: matrix, for which an inverse would be cached
# Returns:
#     a list of (set, get, setInverse, getInverse) functions,
#     used to set/get matrix and its inverse
makeCacheMatrix <- function(mtx = matrix()) {    
    inv <- NULL
    set <- function(pMtx) {
        mtx <<- pMtx
        inv <<- NULL
    }
    get <- function() {
        mtx
    }
    setInverse <- function(inverseM) {
        inv <<- inverseM
    }
    getInverse <- function() {
        inv
    }
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

# Solves for matrix inverse using the cached data
# Computes and updates inverse in cache or retrieves from cache.
# Args:
#     mtxInvCache: a list of functions, returned by makeCacheMatrix
# Returns:
#     inverse of a matrix
cacheSolve <- function(mtxInvCache, ...) {
    inv <- mtxInvCache$getInverse()
    if (!is.null(inv)) {
        message("Getting matrix inverse from cache")
        return(inv)
    }
    mtx <- mtxInvCache$get()
    inv <- solve(mtx)
    mtxInvCache$setInverse(inv)
    inv
}
