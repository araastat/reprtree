#' Plotting representative trees
#' 
#' This function creates either a single plot or a panel of plots for 
#' visualizing representative trees from an ensemble
#' 
#' @param reptree An object of class \code{reprtree)}
#' @param index The index of the reprtree object you want to plot
#' @param all (logical) Do you want to create a panel of plots?
#' @param ncol The number of columns in the plot panel (defaults to NULL)
#' @param nrow The number of rows in the plot panel (defaults to NULL)
#' @param adj 
#' @param main Should a title be placed on the plot (default TRUE)
#' @param ... additional arguments to pass to text.tree. In particular, suppress node labels using \code{label=NULL}
#' @export
#' @S3method plot reprtree
#' @section Details
#' This plot function takes a \code{reprtree} object, and then plots a 
#' single representative tree or a sequence of representative trees (using \code{all=T}.
#' 
#' If only one tree needs to be visualized, the index of the reprtree object to
#' be visualized can be provided.
plot.reprtree <- function(reptree, index = ifelse(all,NULL, 1), all=F,
                          ncol=NULL, nrow=NULL,adj = 0.5, main=T, ...){
  require(plotrix)
  if(!is(reptree,'reprtree')) stop('Wrong class!')
  n <- length(reptree)
  if(all){
    par(mfrow=c(nrow, ncol))
    for(i in 1:n){
      plot(reptree[[i]], type='uniform')
      text(reptree[[i]],adj=adj,cex=0.8, split=F,...)
      labelBG(reptree[[i]])
      labelYN(reptree[[i]])
      if(main) title(main=paste('Tree',names(reptree)[i]))
    }
  } else {
    plot(reptree[[index]], type='uniform') 
    text(reptree[[index]],adj=adj,split=F, cex=0.7, digits=2, ...)
    labelBG(reptree[[index]])
    labelYN(reptree[[index]])
    if(main) title(main=paste('Tree',names(reptree)[index]))
  }
}

labelBG <- function(tr){
  require(plotrix)
  charht <- par('cxy')[2L]
  xy <- tree:::treeco(tr,TRUE)
  nodes <- as.integer(row.names(tr$frame))
  left.child <- match(2*nodes, nodes)
  rows <- tree:::labels.tree(tr)[left.child]
  rows <- gsub('NA,','',rows)
  ind <- !is.na(left.child)
  boxed.labels(xy$x[ind],xy$y[ind]+0.3*charht, rows[ind] , border=F, bg='white',
               cex=0.8, xpad=0.5, ypad=1)
}
labelYN <- function(tr){
  charht <- par('cxy')[2L]
  xy <- tree:::treeco(tr)
  nodes <- as.integer(row.names(tr$frame))
  left.child <- match(2*nodes, nodes)
  ind <- !is.na(left.child)
  text(xy$x[ind]-0.1, xy$y[ind]-0.2*charht, '<< Y',cex=0.6, adj=1)
  text(xy$x[ind]+0.1, xy$y[ind]-0.2*charht, 'N >>', cex=0.6, adj=0)
}

plot.tree <- function (x, y = NULL, type = c("proportional", "uniform"), ...) 
{
  if (inherits(x, "singlenode")) 
    stop("cannot plot singlenode tree")
  if (!inherits(x, "tree")) 
    stop("not legitimate tree")
  type <- match.arg(type)
  uniform <- type == "uniform"
  dev <- dev.cur()
  if (dev == 1L) 
    dev <- 2L
  #assign(paste0("device", dev), uniform, envir = tree_env)
  invisible(treepl(tree:::treeco(x, uniform), node = as.integer(row.names(x$frame)), 
                   ...))
}


treepl <- function (xy, node, erase = FALSE, ...) 
{
  x <- xy$x
  y <- xy$y
  parent <- match((node%/%2L), node)
  sibling <- match(ifelse(node%%2L, node - 1L, node + 1L), 
                   node)
  xx <- rbind(x, x, x[sibling], x[sibling], NA)
  yy <- rbind(y, y[parent], y[parent], y[sibling], NA)
  if (any(erase)) {
    lines(c(xx[, erase]), c(yy[, erase]), col = par("bg"))
    return(x = x[!erase], y = y[!erase])
  }
  plot(range(x), c(min(y)-0.5, max(y)+0.5), type = "n", axes = FALSE, xlab = "", 
       ylab = "")
  text(x[1L], y[1L], "|", ...)
  lines(c(xx[, -1L]), c(yy[, -1L]), ...)
  list(x = x, y = y)
}
