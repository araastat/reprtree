#' Representative trees from ensembles
#' 
#' This package implements the concept of representative trees from ensemble
#' tree-based learners introduced by Banerjee, et al, (2012). Representative trees
#' are, in some sense, trees in the ensemble which are on average the "closest" to 
#' all the other trees in the ensemble. Several trees can be representative of the 
#' ensemble in general. This package currently implements the d2 metric of tree closeness
#' (close in prediction) from Banerjee, et al.
#' 
#' @name reprtree-package
#' @references M. Banerjee, Y. Ding and A-M Noone (2012) "Identifying representative
#'     trees from ensembles". Statistics in Medicine, 31(15):1601-1616.
#' @import randomForest tree
#' @docType package
#' @name reprtree
#' @examples
#' library(reprtree)
#' rforest <- randomForest(Species~., data=iris)
#' reptree <- ReprTree(rforest, iris, metric='d2')
#' plot(reptree, index=1)
NULL


#' Identifying and extracting representative trees from a random forest 
#' 
#' This function takes a random forest object and data to run predictions on
#' and identifies representative trees based on the d0, d1 or d2 metric defined
#' in Banerjee, et al (2012). Currently only the d2 metric is implemented, using
#' either a euclidean distance (for numeric predictions) or a mismatch distance 
#' (for categorical predictions). The average distance D(T) of each tree in the 
#' set of trees in computed, and trees with the lowest D(T) value are extracted
#' and formatted to be compatible with the \code{\link{tree}} class. Trees can then
#' be visualized using \code{plot.tree} and \code{text.tree}, or using a custom
#' plot function (to be defined)
#' 
#' @param rforest A randomForest object
#' @param newdata The data on which predictions will be computed
#' @param metric The metric to be used to evaluate distance between trees. Currently
#'    only the d2 metric is implemented
#' @return A list object containing representations of the representative trees
#'    conformable with the \code{tree} class. Names of the list give the indices
#'    of the representative trees in the set of trees. 
#' @import randomForest tree
#' @export
#' @references M. Banerjee, Y. Ding and A-M Noone (2012) "Identifying representative
#'     trees from ensembles". Statistics in Medicine, 31(15):1601-1616.
#' @examples
#' library(randomForest)
#' library(tree)
#' rforest <- randomForest(Species~., data=iris)
#' reptree <- ReprTree(rforest, iris, metric='d2')
ReprTree <- function(rforest, newdata, metric='d2'){
  if(metric!='d2') stop('invalid metric!')
  require(randomForest)
  print('Constructing distance matrix...')
  preds <- predict2(rforest, newdata=newdata, predict.all=T)
  preds.indiv <- preds$individual
  d <- dist.fn(t(preds.indiv), method=ifelse(rforest$type=='classification',
                                             'mismatch',
                                             'euclidean'))
  print('Finding representative trees...')
  D <- colMeans(d)
  index <- which(D==min(D))
  trees <- lapply(as.list(index), function(i) getTree(rforest, i, labelVar=TRUE))
  names(trees) <- as.character(index)
  trees <- lapply(trees, as.tree, rforest)
  out <- list(trees=trees,D = D)
  class(out) <- c('reprtree','list')
  return(out)
}