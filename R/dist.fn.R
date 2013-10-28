# Compute a distance matrix between rows of a data matrix
# 
# This function takes a matrix or a data.frame, and computes the distance
# between the between the rows of the data matrix. It extends the function 
# \code{\link{dist}} by adding a new metric defined by the proportion of 
# mismatches between two vectors.
# 
# @param x a numeric matrix or data frame
# @param method the distance measure to be used. This must be one of "mismatch",
#      "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#      Any unambiguous substring can be given
# @param ... additional arguments to be passed to the \code{dist} function
# @keywords dist
dist.fn <- function(x, method='mismatch',...){
  METHODS <- c("euclidean", "maximum", "manhattan", "canberra", 
               "binary", "minkowski", "mismatch")
  method <- pmatch(method, METHODS)
  if(is.na(method)) stop("invalid distance method")
  if(METHODS[method] !="mismatch") z <- dist(x,y, method=METHODS[method], ...)
  z = matrix(0, nrow=nrow(x), ncol=nrow(x))
  for(k in 1:(nrow(x)-1)){
    for (l in (k+1):nrow(x)){
      z[k,l] <- mean(x[k,]!=x[l,])
      z[l,k] <- z[k,l]
    }}
  dimnames(z)  <- list(dimnames(x)[[1]],dimnames(x)[[1]])
  return(z)
}
