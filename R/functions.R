#' Convert the result of a getTree call to a format compatible with tree
#' 
#' This function takes the results of a \code{randomForest::getTree} call and 
#' converts the results to a form compatible with \code{tree}
#' @param gTree The results of a call to \code{getTree}
#' @param rforest The randomForest object 
#' @return An object of class \code{tree}, which has a \code{frame} and sufficient
#'     attributes to enable plotting
#' @S3method as tree
as.tree <- function(gTree,rforest){
  if(is.numeric(gTree[,'split var'])) stop("labelVar=T required")
  bl <- matrix("", nrow=nrow(gTree), ncol=3)
  for(row in 1:nrow(gTree)){
    if(row==1){
      bl[row, 1:2] <- c('10','11')
      next
    }
    if(gTree[row,1]>0){
      bl[row,1:2] <- paste0(bl[which(gTree[,1:2]==row,arr.ind=T)], c('0','1'))
    } else {
      bl[row,3] <- bl[which(gTree[,1:2]==row, arr.ind=T)]
    }
  }
  bl <- data.frame(bl, stringsAsFactors=F); names(bl) <- c('left','right','terminal')
  fr <- list()
  fr$var <- as.character(gTree[,"split var"])
  fr$var[is.na(fr$var)] <- '<leaf>'
  fr$n <- fr$dev <- rep(0,length(fr$var))
  fr$yval <- gTree[,'prediction']
  
  # Need to work out split points based on classes of the splitting vars
  classes <- attributes(rforest$terms)$dataClasses
  blah <- data.frame(var=fr$var, splits=as.character(gTree[,'split point']), 
                classes=classes[fr$var], stringsAsFactors=F)
  index <- which(blah$classes=='factor' & !is.na(blah$classes))
  blah$splits[index] <- sapply(blah$splits[index], factor.repr)  
  
  
  splits <- cbind(
    cutleft=paste0(ifelse(blah$classes=='factor' & !is.na(blah$classes),': ','<'),
                   blah$splits), 
    cutright=paste0(ifelse(blah$classes=='factor' & !is.na(blah$classes),
                           ': ','>'),
                           blah$splits))
  splits[fr$var=='<leaf>',] <- ""
  
  fr <- as.data.frame(fr, stringsAsFactors=F)
  fr$splits <- splits
  x <- ifelse(fr$var=='<leaf>', bl[,3], gsub('.{1}$', '', bl[,1]))
  if(rforest$type=='classification'){
    fr$yprob = matrix(1/length(rforest$classes),nrow=nrow(fr), ncol=length(rforest$classes))
  }
  row.names(fr) <- strtoi(x,2)
  fr <- fr[order(x),]
  
  newtr <- list()
  newtr$frame=fr
  attr(newtr,'xlevels') <- rforest$forest$xlevels
  if(rforest$type=='classification') attr(newtr,'ylevels') <- rforest$classes
  class(newtr) <- 'tree'
  return(newtr)
}

#' Compute a distance matrix between rows of a data matrix
#' 
#' This function takes a matrix or a data.frame, and computes the distance
#' between the between the rows of the data matrix. It extends the function 
#' \code{\link{dist}} by adding a new metric defined by the proportion of 
#' mismatches between two vectors.
#' 
#' @param x a numeric matrix or data frame
#' @param method the distance measure to be used. This must be one of "mismatch",
#'      "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#'      Any unambiguous substring can be given
#' @param ... additional arguments to be passed to the \code{dist} function
#' @keywords dist
dist.fn <- function(x, method='mismatch',...){
  METHODS <- c("euclidean", "maximum", "manhattan", "canberra", 
               "binary", "minkowski", "mismatch")
  method <- pmatch(method, METHODS)
  if(is.na(method)) stop("invalid distance method")
  if(METHODS[method] !="mismatch"){
    z <- as.matrix(dist(x, method=METHODS[method], ...))
  } else {
  z = matrix(0, nrow=nrow(x), ncol=nrow(x))
  for(k in 1:(nrow(x)-1)){
    for (l in (k+1):nrow(x)){
      z[k,l] <- mean(x[k,]!=x[l,])
      z[l,k] <- z[k,l]
    }}
  }
   dimnames(z)  <- list(dimnames(x)[[1]],dimnames(x)[[1]])
  return(z)
}

int2bin <- function(x, reverse=F){
  y <- intToBits(x)
  yy <- paste(sapply(strsplit(paste(rev(y)),""),`[[`,2),collapse="")
  out <- gsub('^[0]+','',yy)
  if(reverse){
    bl <- rev(unlist(strsplit(out,'')))
    out <- paste(bl, collapse='')
  }
  return(out)
}

factor.repr <- function(x){
  x <- int2bin(as.integer(x), reverse=T)
  n <- nchar(x)
  paste(letters[1:n][unlist(strsplit(x,''))=='1'],collapse='')
}
