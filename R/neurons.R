#' @title Read bodies from the neuPrint server as skeletons
#'
#' @description  Get \code{nat::neuronlist} objects or data frames in the format of SWC files, for neurons retrievable from a neuPrint server.
#' Choose whether or not to heal the fetched skeletons, assign a soma (if tagged in neuprint) and assign synapses to appxorimate treenode positions, in the style of neuron objects used by the rcatmaid package
#' @param bodyids the body IDs for neurons/segments (bodies) you wish to query
#' @param bodyids a single body ID for a neuron/segment (body) you wish to query
#' @param nat whether or not to read neurons are \code{nat::neuronlist} objects (TRUE) or get SWC data frame (FALSE)
#' @param name whether or not to fetch a name for the given bodyids, using \code{neuprint_get_neuron_names}
#' @param soma whether or not to fetch a possible soma location for the given bodyids, using \code{neuprint_locate_soma}
#' @param heal whether or not to heal a fragmented skeleton using a minimum spanning tree, via \code{heal_skeleton}
#' @param connectors whether or not to add synapse data to the retrieved skeletons in the format used by the \code{rcatmaid} package, for easy use with \code{rcatmaid} or \code{catnat} functions.
#' This can be doen for synapse-less skeletons using \code{neuprint_assign_connectors}
#' @param bodyids the body IDs for neurons/segments (bodies) you wish to query
#' @param dataset optional, a dataset you want to query. If NULL, the default specified by your R environ file is used. See \code{neuprint_login} for details.
#' @param all_segments if TRUE, all bodies are considered, if FALSE, only 'Neurons', i.e. bodies with a status roughly traced status.
#' @param resample if a number, the neuron is resampled using \code{nat::resample}, stepsize = resample. If 0 or FALSE (default), no resampling occurs.
#' @param conn optional, a neuprintr connection object, which also specifies the neuPrint server see \code{?neuprint_login}.
#' If NULL, your defaults set in your R.profile or R.environ are used.
#' @param ... methods passed to \code{neuprint_login}
#' @return a data frame in SWC format, or a neuron/neuronlist object as dictated used by the \code{nat} and \code{rcatmaid} packages
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_get_synapses}}, \code{\link{neuprint_assign_connectors}}
#' @export
#' @rdname neuprint_read_neurons
neuprint_read_neurons <- function(bodyids, name = TRUE, nat = TRUE, soma = TRUE, heal = TRUE, connectors = TRUE, all_segments = TRUE, dataset = NULL, resample = FALSE, conn = NULL, ...){
  neurons = nat::nlapply(bodyids,function(bodyid) neuprint_read_neuron(bodyid=bodyid, nat=nat, soma = soma, heal = heal, connectors = connectors, dataset = dataset, all_segments = all_segments, resample = resample, conn= conn, ...))
  names(neurons) = bodyids
  if(name){
    attr(neurons,"df") = neuprint_get_neuron_names(bodyids = bodyids, dataset = dataset, all_segments = all_segments, conn = conn, ...)
  }else{
    attr(neurons,"df") = data.frame(bodyid=bodyids)
  }
}

#' @export
#' @rdname neuprint_read_neurons
neuprint_read_neuron <- function(bodyid, nat = TRUE, soma = TRUE, heal = TRUE, connectors = TRUE, dataset = NULL, all_segments = TRUE, resample = FALSE, conn = NULL, ...){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  all_segments_json = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf("MATCH (:`%s-%s` {bodyId:%s})-[:Contains]->(:Skeleton)-[:Contains]->(root :SkelNode) WHERE NOT (root)<-[:LinksTo]-() RETURN root.rowNumber AS rowId, root.location.x AS x, root.location.y AS y, root.location.z AS z, root.radius AS radius, -1 AS link ORDER BY root.rowNumber UNION match (:`%s-%s` {bodyId:%s})-[:Contains]->(:Skeleton)-[:Contains]->(s :SkelNode)<-[:LinksTo]-(ss :SkelNode) RETURN s.rowNumber AS rowId, s.location.x AS x, s.location.y AS y, s.location.z AS z, s.radius AS radius, ss.rowNumber AS link ORDER BY s.rowNumber",
                   dataset,
                   all_segments_json,
                   bodyid,
                   dataset,
                   all_segments_json,
                   bodyid)
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, ...)
  if(!length(nc$data)){
    warning("bodyid ", bodyid, " could not be read from ", unlist(getenvoroption("server")))
    return(NULL)
  }
  d = data.frame(do.call(rbind,nc$data))
  d = as.data.frame(t(apply(d,1,function(r) unlist(r))))
  colnames(d) = c("PointNo","X","Y","Z","W","Parent")
  d$Label = 0
  if(heal){
    d = heal_skeleton(x = d)
  }
  n = nat::as.neuron(d)
  if(resample){
    n = nat::resample(x=n,stepsize=resample)
  }
  if(soma){
    somapoint = nat::xyzmatrix(neuprint_locate_soma(bodyids = bodyid, all_segments = all_segments, dataset = dataset, conn = conn, ...))
    near = nabor::knn(query=somapoint,data=nat::xyzmatrix(d),k=1)$nn.idx
    n = nat::as.neuron(nat::as.ngraph(d), origin = c(near))
    d = n$d
  }
  if(connectors){
    synapses = neuprint_get_synapses(bodyid = bodyid, dataset = dataset, roi = "all", conn = conn, ...)
    near = nabor::knn(query= nat::xyzmatrix(synapses),data=nat::xyzmatrix(d),k=1)$nn.idx
    d = list(swc = d, connectors = synapses)
    synapses$treenode_id = n$d[near,"PointNo"]
    n$connectors = synapses
  }
  if(nat){
    n$bodyid = bodyid
    n
  }else{
    d
  }
}

#' @title Assign synapses to a neuronal tree
#'
#' @description  Fetch the synapses associated with a bodyid and assign them to the nearest point in the skeleton. For this to work, the skeletons must be in the same brainsapce as the synaptic data being pulled from neuprint. I.e. do not transform them to another brainspace, untol after this step.
#' @inheritParams neuprint_read_neurons
#' @param x either an object of class neuron, or neuronlist
#' @return a neuron/neuronlist object as dictated used by the \code{nat} and \code{rcatmaid} packages
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_get_synapses}}, \code{\link{neuprint_read_neurons}}
#' @export
#' @rdname neuprint_assign_connectors
neuprint_assign_connectors <-function(x, bodyids = NULL, dataset = NULL, conn = NULL, ...) UseMethod("neuprint_assign_connectors")
neuprint_assign_connectors.neuron <- function(x, bodyids = NULL, dataset = NULL, conn = NULL, ...){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  if(is.null(bodyids)){
    bodyids = x$bodyid
  }
  synapses = neuprint_get_synapses(bodyid = bodyids, dataset = dataset, roi = "all", conn = conn, ...)
  near = nabor::knn(query= nat::xyzmatrix(synapses),data=nat::xyzmatrix(d),k=1)$nn.idx
  synapses$treenode_id = n$d[near,"PointNo"]
  n$connectors = synapses
  n
}
neuprint_assign_connectors.neuronlist  <- function(x, bodyids = NULL, dataset = NULL, conn = NULL, ...){
  nat::nlapply(1:length(x), function(i)
    neuprint_assign_connectors.neuron(x=x[[i]],bodyids=bodyids[i]),dataset=dataset,conn=conn,...)
}

#' @title Heal a fragmented skeleton for a neuron
#'
#' @description  Mend breaks in a skeleton for a neuron, predictign merge sites using the minimum spannign tree method, utilisng \code{igraph::mst}
#' @inheritParams neuprint_read_neurons
#' @param x either an object of class neuron, or neuronlist
#' @return a cohesive SWC like data frame, or a cohesive neuron/neuronlist object as dictated used by the \code{nat} and \code{rcatmaid} packages
#' @seealso \code{\link{neuprint_get_synapses}}, \code{\link{neuprint_read_neurons}}
#' @export
#' @rdname heal_skeleton
heal_skeleton <- function(x, ...){
  n = nat::as.ngraph(x)
  mstree = igraph::mst(graph = n, ...)
  healed = nat::as.neuron(mstree)
  if(nat::is.neuron(x)){
    healed$connectors = x$connectors
    healed$d = merge(healed$d, x$d)
    healed
  }else{
    healed$d
  }
}











