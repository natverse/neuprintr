#' @title Read bodies from the neuPrint server as skeletons
#'
#' @description  Get \code{nat::neuronlist} objects or data frames in the format of SWC files, for neurons retrievable from a neuPrint server.
#' Choose whether or not to heal the fetched skeletons, assign a soma (if tagged in neuprint) and assign synapses to approximate treenode positions, in the style of neuron objects used by the rcatmaid package.
#' If \code{neuprint_read_neuron_simple} is used, just a simple skeleton is retrieved.
#' @param bodyids the body IDs for neurons/segments (bodies) you wish to query
#' @param bodyid a single body ID for a neuron/segment (body) you wish to query
#' @param drvid whether or not to use \code{drvid::read.neuron.dvid} rather than a cypher post request to \code{neuprint_fetch_custom}, in order to read a neuron.
#' This might be faster, and this might also enable access to skeletons on an underlying DVID database that have not yet been ported to neuprint.
#' @param nat whether or not to read neurons are \code{nat::neuronlist} objects (TRUE) or get SWC data frame (FALSE)
#' @param meta whether or not to fetch a meta data for the given bodyids, using \code{neuprint_get_meta}
#' @param soma whether or not to fetch a possible soma location for the given bodyids, using \code{neuprint_locate_soma}
#' @param estimate.soma if soma = TRUE, and estimate.soma = TRUE, then when a soma has not been tagged in the dataset, one is estimated by finding the leaf node with the largest mean geodesic distance from all synapses
#' @param heal whether or not to heal a fragmented skeleton using a minimum spanning tree, via \code{heal_skeleton}
#' @param connectors whether or not to add synapse data to the retrieved skeletons in the format used by the \code{rcatmaid} package, for easy use with \code{rcatmaid} or \code{catnat} functions.
#' This can be done for synapse-less skeletons using \code{neuprint_assign_connectors}
#' @param dataset optional, a dataset you want to query. If NULL, the default specified by your R environ file is used. See \code{neuprint_login} for details.
#' @param all_segments if TRUE, all bodies are considered, if FALSE, only 'Neurons', i.e. bodies with a status roughly traced status.
#' @param resample if a number, the neuron is resampled using \code{nat::resample}, stepsize = resample. If 0 or FALSE (default), no resampling occurs.
#' @param conn optional, a neuprintr connection object, which also specifies the neuPrint server see \code{?neuprint_login}.
#' If NULL, your defaults set in your R.profile or R.environ are used.
#' @inheritParams nat::nlapply
#' @param ... methods passed to \code{neuprint_login}
#' @return a data frame in SWC format, or a \code{nat::neuron}/\code{nat::neuronlist} object as dictated used by the \code{nat} and \code{rcatmaid} packages
#' @examples
#' \donttest{
#' neurons = neuprint_read_neurons(c(818983130, 1796818119))
#' nat::plot3d(neurons, col = "purple", lwd = 2)
#' }
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_get_synapses}}, \code{\link{neuprint_assign_connectors}}
#' @importFrom drvid read.neuron.dvid
#' @export
#' @rdname neuprint_read_neurons
#' @importFrom nat nlapply
neuprint_read_neurons <- function(bodyids,
                                  meta = TRUE,
                                  nat = TRUE,
                                  drvid = FALSE,
                                  soma = TRUE,
                                  estimate.soma = FALSE,
                                  heal = TRUE,
                                  connectors = TRUE,
                                  all_segments = TRUE,
                                  dataset = NULL,
                                  resample = FALSE,
                                  conn = NULL,
                                  OmitFailures = TRUE, ...) {
  neurons = nat::nlapply(id2bit64(bodyids),function(bodyid)
    neuprint_read_neuron(bodyid=bodyid,
                         nat=nat,
                         drvid=drvid,
                         soma = soma,
                         estimate.soma = estimate.soma,
                         heal = heal,
                         connectors = connectors,
                         dataset = dataset,
                         all_segments = all_segments,
                         resample = resample,
                         conn= conn, ...),
    OmitFailures = OmitFailures)
  neurons = neurons[!sapply(neurons,function(n) is.null(n))]
  if(length(neurons)==0){
    stop("Error: none of the given bodyids have skeletons that could be fetched")
  }
  names(neurons) = unlist(sapply(neurons,function(n) n$bodyid))
  if(meta){
    attr(neurons,"df") = neuprint_get_meta(bodyids = names(neurons),
                                           dataset = dataset,
                                           all_segments = all_segments,
                                           conn = conn, ...)
  }else{
    attr(neurons,"df") = data.frame(bodyid=names(neurons))
  }
  neurons
}

#' @export
#' @rdname neuprint_read_neurons
neuprint_read_neuron <- function(bodyid,
                                 nat = TRUE,
                                 drvid = FALSE,
                                 soma = TRUE,
                                 estimate.soma = FALSE,
                                 heal = TRUE,
                                 connectors = TRUE,
                                 dataset = NULL,
                                 all_segments = TRUE,
                                 resample = FALSE,
                                 conn = NULL, ...){
  all_segments_json = ifelse(all_segments,"Segment","Neuron")
  if(drvid){
    n = drvid::read.neuron.dvid(bodyid)
    d = n$d
  }else{
    n = neuprint_read_neuron_simple(id2bit64(bodyid), dataset=dataset,
                                    conn = conn, heal = FALSE,...)
  }
  if(heal){
    n = suppressWarnings( heal_skeleton(x = n) )
    d = n$d
  }
  if(resample){
    n = nat::resample(x=n,stepsize=resample)
  }
  if(soma){
    somapoint = tryCatch(nat::xyzmatrix(neuprint_locate_soma(bodyids = bodyid, all_segments = all_segments, dataset = dataset, conn = conn, ...)),
                         error = function(e) NA)
    if(sum(is.na(somapoint))==0){
      near.soma = nabor::knn(query=somapoint,data=nat::xyzmatrix(n$d),k=1)$nn.idx
    }else if (estimate.soma){ # Quickly stimate soma location as the leave node furthest from synapses, or other leaf nodes
      leaves = ends = nat::endpoints(n)
      far.leaves = nabor::knn(query=nat::xyzmatrix(n$d[leaves,]),data=nat::xyzmatrix(n$d[leaves,]),k=10)$nn.dist
      leaves = ends = leaves[which(far.leaves[,10]>mean(far.leaves[,10]))]
      dists = sapply(leaves, function(l) mean(sapply(igraph::all_shortest_paths(graph = nat::as.ngraph(d),
                                   from = l,
                                   to = ends,
                                   mode = c("all"),weights = NULL)$res,length)))
      near.soma = leaves[which.max(dists)]
    }else{
      leaves = nat::endpoints(n)
      far.leaves = nabor::knn(query=nat::xyzmatrix(n$d[leaves,]),data=nat::xyzmatrix(n$d[leaves,]),k=100)$nn.dist
      near.soma = leaves[which.max(mean(far.leaves[,100]))]
    }
    near.soma = n$d[c(near.soma),"PointNo"]
    n = nat::as.neuron(nat::as.ngraph(n$d), origin = c(near.soma))
    n$d$Label[near.soma] = 1
    d = n$d
  }
  if(connectors){
    synapses = neuprint_get_synapses(bodyids = bodyid, dataset = dataset, roi = NULL, conn = conn, ...)
    near = nabor::knn(query= nat::xyzmatrix(synapses),data=nat::xyzmatrix(n$d),k=1)$nn.idx
    synapses$treenode_id = n$d[near,"PointNo"]
    synapses = synapses[,c("treenode_id","connector_id", "prepost", "x", "y", "z", "confidence", "bodyid", "partner")]
    n$connectors = synapses
  }
  if(nat){
    n$bodyid = bodyid
    class(n) = union(c("neuprintneuron", "catmaidneuron"), class(n))
    n
  }else{
    if(connectors){
      list(swc = d, connectors = synapses)
    }else{
      d
    }
  }
}

#' @title Assign synapses to a neuronal tree
#'
#' @description  Fetch the synapses associated with a bodyid and assign them to the nearest point in the skeleton. For this to work, the skeletons must be in the same brainspace as the synaptic data being pulled from neuprint. I.e. do not transform them to another brainspace, until after this step.
#' @inheritParams neuprint_read_neurons
#' @param x either an object of class neuron, or neuronlist
#' @return a neuron/neuronlist object as dictated used by the \code{nat} and \code{rcatmaid} packages
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_get_synapses}}, \code{\link{neuprint_read_neurons}}
#' @export
#' @rdname neuprint_assign_connectors
neuprint_assign_connectors <-function(x, bodyids = NULL, dataset = NULL, conn = NULL, ...) UseMethod("neuprint_assign_connectors")
neuprint_assign_connectors.neuron <- function(x, bodyids = NULL, dataset = NULL, conn = NULL, ...){
  if(is.null(bodyids)){
    bodyids = x$bodyid
  }
  synapses = neuprint_get_synapses(bodyids = bodyids, dataset = dataset, roi = "all", conn = conn, ...)
  near = nabor::knn(query= nat::xyzmatrix(synapses),data=nat::xyzmatrix(x$d),k=1)$nn.idx
  synapses$treenode_id = n$d[near,"PointNo"]
  n$connectors = synapses
  n
}
neuprint_assign_connectors.neuronlist  <- function(x, bodyids = names(x), dataset = NULL, conn = NULL, ...){
  nat::nlapply(1:length(x), function(i)
    neuprint_assign_connectors.neuron(x=x[[i]],bodyids=bodyids[i]),dataset=dataset,conn=conn,...)
}

#' @title Heal a fragmented skeleton for a neuron
#'
#' @description  Mend breaks in a skeleton for a neuron, predicting merge sites using the minimum spanning tree method, utilising \code{igraph::mst}
#' @inheritParams neuprint_read_neurons
#' @param x either an object of class neuron, or neuronlist
#' @return a cohesive SWC like data frame, or a cohesive neuron/neuronlist object as dictated used by the \code{nat} and \code{rcatmaid} packages
#' @seealso \code{\link{neuprint_get_synapses}}, \code{\link{neuprint_read_neurons}}
#' @export
#' @importFrom igraph mst
heal_skeleton <- function(x, ...){
  n = nat::as.ngraph(x)
  mstree = igraph::mst(graph = n, ...)
  healed = nat::as.neuron(mstree)
  if(healed$nTrees>1){
    fragmented = break_into_subtress(healed)
    healed = stitch_neurons(x=fragmented)
  }
  if(nat::is.neuron(x)){
    healed$connectors = x$connectors
    #healed$d = merge(healed$d, x$d)
    healed
  }else{
    healed$d
  }
}

#' @export
#' @rdname neuprint_read_neurons
#' @examples
#' \donttest{
#' dl1.info <- neuprint_search('.*mPN.*DL1.*')
#' dl1.info
#' dl1s=neuprint_read_neuron_simple(dl1.info$bodyid)
#' plot(dl1s, WithNode=F)
#' }
neuprint_read_neuron_simple <- function(bodyid, dataset=NULL, conn=NULL, heal=TRUE, ...) {
  bodyid=as.character(id2bit64(bodyid))
  if(length(bodyid)>1) {
    fakenl=structure(bodyid, .Names=bodyid)
    nl=nat::nlapply(fakenl, neuprint_read_neuron_simple, dataset=dataset, conn=conn, ...)
    return(nl)
  }
  dataset=check_dataset(dataset)
  path=file.path("api/skeletons/skeleton", dataset, bodyid)
  res=neuprint_fetch(path, conn=conn, simplifyVector = TRUE, include_headers = FALSE, ...)
  colnames(res$data)=c("PointNo","X","Y","Z","W","Parent")
  df=as.data.frame(res$data)
  # convert radius to diameter
  df$W=df$W*2
  n=nat::as.neuron(df)
  if(heal) heal_skeleton(n) else n
}
