#' Plotting representative trees
#' 
#' This function creates either a single plot or a panel of plots for 
#' visualizing representative trees from an ensemble
#' 
#' @param reptree An object of class \code{reprtree)}
#' @param all (logical) Do you want to create a panel of plots?
#' @param index The index of the reprtree object you want to plot
#' @param ncol The number of columns in the plot panel (defaults to NULL)
#' @param nrow The number of rows in the plot panel (defaults to NULL)
#' @section Details
#' This plot function takes a \code{reprtree} object, and then either plots a 
#' single representative tree or a panel of all the representative trees. If
#' a panel is desired and neither nrow nor ncol are provided, they are automatically 
#' chosen. If one of nrow or ncol is specified, the other is computed to make the 
#' most compact panel. 
#' 
#' If only one tree needs to be visualized, the index of the reprtree object to
#' be visualized can be provided.
plot.reprtree <- function(reptree, all=FALSE, index = ifelse(all,NULL, 1),
                          ncol=NULL, nrow=NULL){
  if(is(reptree,'reprtree')) stop('Wrong class!')
  n <- length(reptree)
  if(all){
    if(is.null(ncol) & is.null(nrow)){
    ncol <- floor(sqrt(n))
    nrow <- ceil(n/ncol)
    } else if(is.null(ncol) & !is.null(nrow)){
      ncol <- ceil(n/nrow)
    } else if(is.null(nrow) &  !is.null(ncol)){
      nrow <- ceil(n/ncol)
    } 
    if(nrow * ncol < n) stop('Panel dimensions are too small')    
    opar <- par()
    par(mfrow=c(nrow, ncol))
    for(i in 1:n){
      plot(reptree[[i]], type='uniform', main=names(reptree)[i]) 
      text(reptree[[i]])
    }
    par(opar)
  } else {
    plot(reptree[[index]], type='uniform', main=names(reptree)[index]) 
    text(reptree[[index]])
  }
}