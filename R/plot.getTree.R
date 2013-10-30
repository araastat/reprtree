#' Plotting a tree extracted from a random forest
#' 
#' @param rforest A \code{randomForest} object
#' @param tr A \code{tree} object. Either rforest or tr must be input
#' @param k The index of the tree to be plotted
#' @param depth The depth of the tree to be plotted
#' @param main The title to put on the graph
#' @param ... Additional parameters to be passed to \code{text.tree}
#' @export
#' @examples
#' library(randomForest)
#' rforest <- randomForest(Species~., data=iris, ntree=20)
#' plot.getTree(rforest, k=3, depth=4)
plot.getTree <- function(rforest=NULL,tr=NULL,k=1, depth=0,main=NULL, ...){
  require(randomForest)
  if(is.null(rforest) && is.null(tr))stop('One of a random forest object or a tree object must be input')
  if(!is.null(rforest)){
    gTree <- getTree(rforest, k=k, labelVar=TRUE)
    x <- as.tree(gTree, rforest)
  } else {
    x <- tr
  }
  if(depth>0){
    x <- snip.depth(x,depth)
  }
  plot(x, type='uniform')
  text(x,split=FALSE,...)
  labelBG(x)
  labelYN(x)
  title(main=main)
}