# hidden
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

# hidden
break_into_subtress <- function(x){
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

# hidden, from elmr
stitch_neurons <- function (x, prefer_soma = FALSE, sort = TRUE, warndist = 100000){
  if (!nat::is.neuronlist(x))
    stop("x must be a neuronlist object!")
  if (length(x) <= 1)
    return(x)
  if (prefer_soma) {
    svec = sapply(x, has_soma)
  }
  else {
    svec = rep(0, length(x))
  }
  if (sort) {
    nnodes = sapply(x, function(n) nrow(n$d))
    eps = 1/(max(nnodes) + 1)
    svec = (eps + svec) * nnodes
  }
  if (any(svec > 0))
    x = x[order(svec, decreasing = TRUE)]
  if (length(x) == 2)
    return(stitch_neuron(x[[1]], x[[2]]))
  dists = sapply(x[-1], function(n) closest_ends(x[[1]], n)$dist)
  mindist = min(dists)
  if (isTRUE(is.finite(warndist)) && mindist > warndist) {
    warning("Suspicious minimum distance between fragments ( ",
            mindist, ")!")
  }
  chosen = which.min(dists) + 1
  x[[1]] = stitch_neuron(x[[1]], x[[chosen]])
  stitch_neurons(x[-chosen], prefer_soma = FALSE, sort = FALSE)
}

# hidden, from elmr
stitch_neuron <- function (a, b) {
  if (any(a$d$PointNo %in% b$d$PointNo)) {
    b$d$PointNo = b$d$PointNo + max(a$d$PointNo)
    b$d$Parent = b$d$Parent + max(a$d$PointNo)
  }
  ag = nat::as.ngraph(a)
  bg = nat::as.ngraph(b)
  abg = nat::as.ngraph(igraph::disjoint_union(ag, bg))
  ce = closest_ends(a, b)
  a_pointno = a$d$PointNo[ce$a_idx]
  b_pointno = b$d$PointNo[ce$b_idx]
  node_label = intersect(c("name", "label"), igraph::list.vertex.attributes(ag))[1]
  if (all(is.na(node_label)))
    stop("Graph nodes are not labelled!")
  abg = abg + igraph::edge(which(igraph::vertex_attr(abg, node_label) ==
                                   a_pointno), which(igraph::vertex_attr(abg, node_label) ==
                                                       b_pointno))
  nat::as.neuron(nat::as.ngraph(abg))
}

# hidden, from elmr
closest_ends <- function (a, b) {
  epa = nat::endpoints(a)
  epb = nat::endpoints(b)
  axyz = a$d[epa, c("X", "Y", "Z")]
  bxyz = b$d[epb, c("X", "Y", "Z")]
  nnres = nabor::knn(axyz, bxyz, k = 1)
  b_idx = which.min(nnres$nn.dists)
  a_idx = nnres$nn.idx[b_idx, 1]
  return(list(a_idx = epa[a_idx], b_idx = epb[b_idx], dist = min(nnres$nn.dists)))
}
