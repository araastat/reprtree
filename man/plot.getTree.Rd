\name{plot.getTree}
\alias{plot.getTree}
\title{Plotting a tree extracted from a random forest}
\usage{
  plot.getTree(rforest, k = 1, depth = 0, ...)
}
\arguments{
  \item{rforest}{A \code{randomForest} object}

  \item{k}{The index of the tree to be plotted}

  \item{depth}{The depth of the tree to be plotted}

  \item{...}{Additional parameters to be passed to
  \code{text.tree}}
}
\description{
  Plotting a tree extracted from a random forest
}
\examples{
library(randomForest)
rforest <- randomForest(Species~., data=iris, ntree=20)
plot.getTree(rforest, k=3, depth=4)
}

