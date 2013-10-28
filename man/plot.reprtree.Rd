\name{plot.reprtree}
\alias{plot.reprtree}
\title{Plotting representative trees}
\usage{
  plot.reprtree(reptree, index = ifelse(all, NULL, 1),
    all = F, ncol = NULL, nrow = NULL, adj = 0.5, main = T,
    ...)
}
\arguments{
  \item{reptree}{An object of class \code{reprtree)}}

  \item{index}{The index of the reprtree object you want to
  plot}

  \item{all}{(logical) Do you want to create a panel of
  plots?}

  \item{ncol}{The number of columns in the plot panel
  (defaults to NULL)}

  \item{nrow}{The number of rows in the plot panel
  (defaults to NULL)}

  \item{adj}{}

  \item{main}{Should a title be placed on the plot (default
  TRUE)}

  \item{...}{additional arguments to pass to text.tree. In
  particular, suppress node labels using \code{label=NULL}}
}
\description{
  This function creates either a single plot or a panel of
  plots for visualizing representative trees from an
  ensemble
}
\section{Details
This plot function takes a \code{reprtree} object, and then plots a
single representative tree or a sequence of representative trees (using \code{all=T}.

If only one tree needs to be visualized, the index of the reprtree object to
be visualized can be provided.}{

}

