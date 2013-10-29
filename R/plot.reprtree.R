#' Plotting representative trees
#' 
#' This function creates either a single plot or a panel of plots for 
#' visualizing representative trees from an ensemble
#' 
#' @param reptree An object of class \code{reprtree)}
#' @param index The index of the reprtree object you want to plot
#' @param all (logical) Do you want to create a panel of plots?
#' @param ncol The number of columns in the plot panel (default is NULL)
#' @param nrow The number of rows in the plot panel (default is NULL)
#' @param adj 
#' @param main Title of plot (default is NULL)
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
                          ncol=NULL, nrow=NULL,adj = 0.5, main=NULL, ...){
  require(plotrix)
  if(!is(reptree,'reprtree')) stop('Wrong class!')
  n <- length(reptree)
  if(all){
    par(mfrow=c(nrow, ncol))
    for(i in 1:n){
      plot(reptree[[i]], type='uniform')
      text(reptree[[i]],adj=adj,cex=0.7, split=F,...)
      labelBG(reptree[[i]])
      labelYN(reptree[[i]])
      if(main) title(main=paste('Tree',names(reptree)[i]))
    }
  } else {
    plot(reptree[[index]], type='uniform') 
    xy <- tree:::treeco(reptree[[index]], uniform=TRUE)
    text(reptree[[index]],adj=adj,split=F, cex=0.7, digits=2, ...)
    labelBG(reptree[[index]])
    labelYN(reptree[[index]])
    title(main=main)
  }
}

labelBG <- function(tr){
  require(plotrix)
  charht <- par('cxy')[2L]
  xy <- tree:::treeco(tr,uniform=TRUE)
  nodes <- as.integer(row.names(tr$frame))
  left.child <- match(2*nodes, nodes)
  rows <- tree:::labels.tree(tr)[left.child]
  rows <- gsub('NA,','',rows)
  ind <- !is.na(left.child)
  boxed.labels(xy$x[ind],xy$y[ind]+0.5*charht, rows[ind] , border=F, bg='white',
               cex=0.8, xpad=0.5, ypad=1)
}
labelYN <- function(tr){
  charht <- par('cxy')[2L]
  xy <- tree:::treeco(tr, uniform=T)
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
  # Modified from tree:::treepl
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

#' Annotate a Tree Plot
#' 
#' @S3method text tree
#' @description Add text to a tree plot. Modification of \code{tree:::text.tree} to add uniform type of tree.
#' 
#' @param x an object of class "tree"
#' @param splits logical. If \code{TRUE} the splits are labelled
#' @param label The name of column in the \code{frame} component of \code{x}, to be used to label the nodes. Can be \code{NULL} to suppress node-labelling
#' @param all logical. By default, only the leaves are labelled, but if true interior nodes are also labelled
#' @param pretty the manipulation used for split labels infolving attributes. See Details.
#' @param digits significant digits for numerical labels
#' @param adj,xpd,... graphical parameters such as \code{cex} and \code{font}
#' @param uniform logical. Is \code{plot(..., type='uniform')} used to create the tree plot?
#' 
#' @details
#' If pretty = 0 then the level names of a factor split attributes are used unchanged. 
#' If pretty = NULL, the levels are presented by a, b, ... z, 0 ... 5. 
#' If pretty is a positive integer, abbreviate is applied to the labels with that value for its argument minlength.
#' 
#' If the lettering is vertical (par srt = 90) and adj is not supplied it is adjusted appropriately.
#' 
#' @author Abhijit Dasgupta, modifying original code by B.D. Ripley
#' @seealso \link{\code{plot.tree}}
text.tree <- function (x, splits = TRUE, label = "yval", all = FALSE, pretty = NULL, 
          digits = getOption("digits") - 3, adj = par("adj"), xpd = TRUE, uniform=T,
          ...) 
{
  oldxpd <- par(xpd = xpd)
  on.exit(par(oldxpd))
  if (inherits(x, "singlenode")) 
    stop("cannot plot singlenode tree")
  if (!inherits(x, "tree")) 
    stop("not legitimate tree")
  frame <- x$frame
  column <- names(frame)
  if (!is.null(ylevels <- attr(x, "ylevels"))) 
    column <- c(column, ylevels)
  if (!is.null(label) && is.na(match(label, column))) 
    stop("label must be a column label of the frame component of the tree")
  charht <- par("cxy")[2L]
  if (!is.null(srt <- list(...)$srt) && srt == 90) {
    if (missing(adj)) 
      adj <- 0
    ladj <- 1 - adj
  }
  else ladj <- adj
  xy <- tree:::treeco(x, uniform=uniform)
  if (splits) {
    node <- as.integer(row.names(frame))
    left.child <- match(2 * node, node)
    rows <- labels.tree(x, pretty = pretty)[left.child]
    ind <- !is.na(rows)
    text(xy$x[ind], xy$y[ind] + 0.5 * charht, rows[ind], 
         adj = adj, ...)
  }
  if (!is.null(label)) {
    leaves <- if (all) 
      rep(TRUE, nrow(frame))
    else frame$var == "<leaf>"
    if (label == "yval" & !is.null(ylevels)) 
      stat <- as.character(frame$yval[leaves])
    else if (!is.null(ylevels) && !is.na(lev <- match(label, 
                                                      ylevels))) 
      stat <- format(signif(frame$yprob[leaves, lev], digits = digits))
    else stat <- format(signif(frame[leaves, label], digits = digits))
    if (!is.null(dim(stat)) && dim(stat)[2L] > 1) {
      if (length(dimnames(stat)[[2L]])) 
        stat[1L, ] <- paste(sep = ":", dimnames(stat)[[2L]], 
                            stat[1L, ])
      stat <- do.call("paste", c(list(sep = "\n"), split(stat, 
                                                         col(stat))))
    }
    text(xy$x[leaves], xy$y[leaves] - 0.5 * charht, labels = stat, 
         adj = ladj, ...)
  }
  invisible()
}