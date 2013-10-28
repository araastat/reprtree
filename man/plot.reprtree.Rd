\name{plot.reprtree}
\alias{plot.reprtree}
\title{Plotting representative trees}
\usage{
  plot.reprtree(reptree, all = FALSE,
    index = ifelse(all, NULL, 1), ncol = NULL, nrow = NULL)
}
\arguments{
  \item{reptree}{An object of class \code{reprtree)}}

  \item{all}{(logical) Do you want to create a panel of
  plots?}

  \item{index}{The index of the reprtree object you want to
  plot}

  \item{ncol}{The number of columns in the plot panel
  (defaults to NULL)}

  \item{nrow}{The number of rows in the plot panel
  (defaults to NULL)}
}
\description{
  This function creates either a single plot or a panel of
  plots for visualizing representative trees from an
  ensemble
}
\section{Details
This plot function takes a \code{reprtree} object, and then either plots a
single representative tree or a panel of all the representative trees. If
a panel is desired and neither nrow nor ncol are provided, they are automatically
chosen. If one of nrow or ncol is specified, the other is computed to make the
most compact panel.

If only one tree needs to be visualized, the index of the reprtree object to
be visualized can be provided.}{

}

