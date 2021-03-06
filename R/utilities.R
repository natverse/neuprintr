# hidden
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

# hidden
break_into_subtrees <- function(x){
  nlist = nat::neuronlist()
  for(i in 1:x$nTrees){
    segs = x$SubTrees[[i]]
    d = x$d[unique(unlist(segs)),]
    rownames(d) = 1:nrow(d)
    n = nat::as.neuron(d)
    nlist  = c(nlist, nat::as.neuronlist(n))
  }
  nlist
}

# hidden, from elmr
has_soma<-function(x){
  !is.null(x$tags$soma)
}


#' @importFrom nat progress_natprogress
#' @export
nat::progress_natprogress
