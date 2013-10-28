\name{dist.fn}
\alias{dist.fn}
\title{Compute a distance matrix between rows of a data matrix}
\usage{
  dist.fn(x, method = "mismatch", ...)
}
\arguments{
  \item{x}{a numeric matrix or data frame}

  \item{method}{the distance measure to be used. This must
  be one of "mismatch", "euclidean", "maximum",
  "manhattan", "canberra", "binary" or "minkowski".  Any
  unambiguous substring can be given}

  \item{...}{additional arguments to be passed to the
  \code{dist} function}
}
\description{
  This function takes a matrix or a data.frame, and
  computes the distance between the between the rows of the
  data matrix. It extends the function \code{\link{dist}}
  by adding a new metric defined by the proportion of
  mismatches between two vectors.
}
\examples{
library(randomForest)
rforest <- randomForest(Species~., data=iris)
preds <- predict(rforest, newdata=iris, predict.all=TRUE)
d <- dist.fn(t(preds$individual), method='mismatch')
}
\keyword{dist}

