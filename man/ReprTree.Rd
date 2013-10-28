\name{ReprTree}
\alias{ReprTree}
\title{Identifying and extracting representative trees from a random forest}
\usage{
  ReprTree(rforest, newdata, metric = "d2")
}
\arguments{
  \item{rforest}{A randomForest object}

  \item{newdata}{The data on which predictions will be
  computed}

  \item{metric}{The metric to be used to evaluate distance
  between trees. Currently only the d2 metric is
  implemented}
}
\value{
  A list object containing representations of the
  representative trees conformable with the \code{tree}
  class. Names of the list give the indices of the
  representative trees in the set of trees.
}
\description{
  This function takes a random forest object and data to
  run predictions on and identifies representative trees
  based on the d0, d1 or d2 metric defined in Banerjee, et
  al (2012). Currently only the d2 metric is implemented,
  using either a euclidean distance (for numeric
  predictions) or a mismatch distance (for categorical
  predictions). The average distance D(T) of each tree in
  the set of trees in computed, and trees with the lowest
  D(T) value are extracted and formatted to be compatible
  with the \code{\link{tree}} class. Trees can then be
  visualized using \code{plot.tree} and \code{text.tree},
  or using a custom plot function (to be defined)
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

