#' Plotting a tree extracted from a random forest
#' 
#' @param rforest A \code{randomForest} object
#' @param k The index of the tree to be plotted
#' @param depth The depth of the tree to be plotted
#' @param ... Additional parameters to be passed to \code{text.tree}
#' @examples
#' library(randomForest)
#' rforest <- randomForest(Species~., data=iris, ntree=20)
#' plot.getTree(rforest, k=3, depth=4)
plot.getTree <- function(rforest,k=1, depth=0, ...){
  require(randomForest)
  gTree <- getTree(rforest, k=k, labelVar=TRUE)
  x <- as.tree(gTree, rforest)
  if(depth>0){
    x <- snip.depth(x,depth)
  }
  plot(x, type='uniform')
  text(x,split=FALSE,...)
  labelBG(x)
  labelYN(x)
}