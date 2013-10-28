\docType{package}
\name{reprtree-package}
\alias{reprtree-package}
\title{Representative trees from ensembles}
\description{
  This package implements the concept of representative
  trees from ensemble tree-based learners introduced by
  Banerjee, et al, (2012). Representative trees are, in
  some sense, trees in the ensemble which are on average
  the "closest" to all the other trees in the ensemble.
  Several trees can be representative of the ensemble in
  general. This package currently implements the d2 metric
  of tree closeness (close in prediction) from Banerjee, et
  al
}
\examples{
library(randomForest)
library(tree)
rforest <- randomForest(Species~., data=iris)
reptree <- ReprTree(rforest, iris, metric='d2')
}
\references{
  M. Banerjee, Y. Ding and A-M Noone (2012) "Identifying
  representative trees from ensembles". Statistics in
  Medicine, 31(15):1601-1616.
}

