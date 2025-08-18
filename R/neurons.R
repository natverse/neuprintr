#' @title Read skeleton and optional synapse information for neuPrint bodies
#'
#' @description \code{neuprint_read_neurons} is a full service function that
#'   tries to build as complete \code{\link{neuron}} objects as possible. Choose
#'   whether or not to heal the fetched skeletons, assign a soma (if tagged in
#'   neuprint) and assign synapses to approximate treenode positions, in the
#'   style of neuron objects used by the rcatmaid package.
#'
#'   If \code{neuprint_read_skeletons} is used, just a simple skeleton is
#'   retrieved.
#' @param bodyids the body IDs for neurons/segments (bodies) you wish to query.
#'   This can be in any form understood by \code{\link{neuprint_ids}}.
#' @param drvid whether or not to use \code{drvid::read.neuron.dvid} rather than
#'   a cypher post request to \code{neuprint_fetch_custom}, in order to read a
#'   neuron. This might be faster, and this might also enable access to
#'   skeletons on an underlying DVID database that have not yet been ported to
#'   neuprint.
#' @param nat whether or not to read neurons as \code{nat::\link[nat]{neuron}}
#'   objects (TRUE) or get SWC data frame (FALSE)
#' @param meta whether or not to fetch a meta data for the given bodyids, using
#'   \code{neuprint_get_meta}
#' @param soma whether or not to fetch a possible soma location for the given
#'   bodyids, using \code{neuprint_locate_soma}
#' @param heal whether or not to heal a fragmented skeleton using a minimum
#'   spanning tree, via \code{nat::stitch_neurons_mst}
#' @param heal.threshold distance in raw units beyond which isolated fragments
#'   will not be merged onto the main skeleton. The default of \code{1000}
#'   implies 8000 nm for the hemibrain dataset. Use \code{Inf} to merge all
#'   fragments.
#' @param connectors whether or not to add synapse data to the retrieved
#'   skeletons in the format used by the \code{rcatmaid} package, for easy use
#'   with \code{rcatmaid} or \code{catnat} functions. This can be done for
#'   synapse-less skeletons using \code{neuprint_assign_connectors}
#' @param all_segments if TRUE, all bodies are considered, if FALSE, only
#'   'Neurons', i.e. bodies with a status roughly traced status.
#' @param resample if a number, the neuron is resampled using
#'   \code{nat::resample}, stepsize = resample. If 0 or FALSE (default), no
#'   resampling occurs.
#' @inherit neuprint_fetch_custom params
#' @inheritParams nat::nlapply
#' @param ... methods passed to \code{neuprint_login}
#' @return \code{nat::\link[nat]{neuronlist}} containing either
#'   \code{nat::\link[neuron]{neuron}} objects as used by the \code{nat} and
#'   \code{catmaid} packages (when \code{nat=TRUE}) \emph{or}, for each neuron,
#'   a list of length containing two data frames (\code{swc,connectorts}in SWC
#'   format. /
#'
#' @examples
#' \donttest{
#' neurons = neuprint_read_neurons(c(818983130, 1796818119))
#' library(nat)
#' plot3d(neurons, col = "purple", lwd = 2)
#'
#' ## In this example, in hemibrain:v1.0, an erroneously connected bit
#' ## of neuron skeleton is removed.
#' n1 = neuprint_read_neurons(5812980863, heal.threshold = 20e3)
#' ## compare with
#' n2 = neuprint_read_neurons(5812980863, heal = FALSE)
#' }
#' @seealso \code{\link{neuprint_fetch_custom}},
#'   \code{\link{neuprint_get_synapses}},
#'   \code{\link{neuprint_assign_connectors}}
#' @importFrom drvid read.neuron.dvid
#' @export
#' @importFrom nat nlapply
neuprint_read_neurons <- function(bodyids,
                                  meta = TRUE,
                                  nat = TRUE,
                                  drvid = FALSE,
                                  soma = TRUE,
                                  heal = TRUE,
                                  heal.threshold=1000,
                                  connectors = TRUE,
                                  all_segments = TRUE,
                                  dataset = NULL,
                                  resample = FALSE,
                                  conn = NULL,
                                  OmitFailures = TRUE,
                                  ...) {
  bodyids = neuprint_ids(bodyids, conn = conn, dataset = dataset)
  if(length(bodyids)==0)
    stop("No bodyids to fetch!")
  neurons = nat::nlapply(bodyids,function(bodyid)
    neuprint_read_neuron(bodyid=bodyid,
                         nat=nat,
                         drvid=drvid,
                         soma = soma,
                         heal = heal,
                         heal.threshold=heal.threshold,
                         connectors = connectors,
                         dataset = dataset,
                         all_segments = all_segments,
                         resample = resample,
                         conn= conn,
                         ...),
    OmitFailures = OmitFailures)
  neurons = neurons[!sapply(neurons,function(n) is.null(n))]
  names(neurons) = unlist(sapply(neurons,function(n) n$bodyid))
  if(length(neurons)==0){
    stop("Error reading bodyids. Likely no valid ids or no connection to neuPrint!")
  }else if(!all(bodyids%in%names(neurons))){
    missed = setdiff(bodyids,names(neurons))
    warning("Dropping given bodyids that could not be read from ", neuprint_login(conn=conn)$server," : ", paste(missed, collapse = ", "))
  }
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

# hidden
neuprint_read_neuron <- function(bodyid,
                                 nat = TRUE,
                                 drvid = FALSE,
                                 soma = TRUE,
                                 heal = TRUE,
                                 heal.threshold=1000,
                                 connectors = TRUE,
                                 dataset = NULL,
                                 all_segments = TRUE,
                                 resample = FALSE,
                                 conn = NULL, ...){
  all_segments_json = ifelse(all_segments,"Segment","Neuron")
  if(drvid){
    n = tryCatch(drvid::read.neuron.dvid(bodyid),error = function(e) {warning(e); NULL})
    d = n$d
  }else{
    n = tryCatch(neuprint_read_skeletons(id2char(bodyid), dataset=dataset,conn = conn, heal = heal,heal.threshold=heal.threshold,...),error = function(e) {warning(e); NULL})
    d = n$d
  }
  if(is.null(n)){
    warning("Failed to read neuron ", bodyid , " from ", neuprint_login(conn=conn)$server,", dropping ...")
  }
  if(resample){
    n = nat::resample(x=n,stepsize=resample)
  }
  if(connectors){
    synapses = neuprint_get_synapses(bodyids = bodyid, dataset = dataset, roi = NULL, conn = conn, ...)
  }else{
    synapses = NULL
  }
  if(soma){
    somapoint = tryCatch(nat::xyzmatrix(neuprint_locate_soma(bodyids = bodyid, all_segments = all_segments, dataset = dataset, conn = conn, ...)),
                         error = function(e) NA)
    if(sum(is.na(somapoint))==0){
      near.soma = nabor::knn(query=somapoint,data=nat::xyzmatrix(n$d),k=1)$nn.idx
      n = nat::as.neuron(nat::as.ngraph(n$d), origin = c(near.soma))
      n$d$Label[near.soma] = 1
      n$soma = n$d$PointNo[near.soma]
      n$tags$soma = n$soma
      d = n$d
    }else{
      n$soma = n$tags$soma = NA
    }
  }else{
    n$soma = n$tags$soma = NA
  }
  if(isTRUE(!is.null(synapses) && nrow(synapses))){
    near = nabor::knn(query= nat::xyzmatrix(synapses),data=nat::xyzmatrix(n$d),k=1)
    synapses$treenode_id = n$d[near$nn.idx,"PointNo"]
    synapses = synapses[near$nn.dists<1000,] # remove erroneously associated synapses
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

#'@export
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

#'@export
neuprint_assign_connectors.neuronlist  <- function(x, bodyids = names(x), dataset = NULL, conn = NULL, ...){
  nat::nlapply(1:length(x), function(i)
    neuprint_assign_connectors.neuron(x=x[[i]],bodyids=bodyids[i]),dataset=dataset,conn=conn,...)
}


#' @export
#' @rdname neuprint_read_neurons
#' @examples
#' \donttest{
#' dl1s=neuprint_read_skeletons('DL1 adPN')
#' plot(dl1s, WithNode=FALSE)
#' }
neuprint_read_skeletons <- function(bodyids, dataset=NULL, conn=NULL, heal=TRUE,
                                    heal.threshold=1000, ...) {
  dataset = check_dataset(dataset, conn=conn)
  bodyids=neuprint_ids(bodyids, conn = conn, dataset = dataset)
  if(length(bodyids)>1) {
    fakenl=structure(bodyids, .Names=bodyids)
    nl=nat::nlapply(fakenl, neuprint_read_skeletons, dataset=dataset, conn=conn, heal=heal, heal.threshold=heal.threshold, ...)
    return(nl)
  }
  path=file.path("api/skeletons/skeleton", dataset, bodyids)
  res=neuprint_fetch(path, conn=conn, simplifyVector = TRUE, include_headers = FALSE, ...)
  colnames(res$data)=c("PointNo","X","Y","Z","W","Parent")
  df=as.data.frame(res$data)
  # convert radius to diameter
  df$W=df$W*2
  n=nat::as.neuron(df)
  n$bodyid = bodyids
  if(heal) suppressMessages(nat::stitch_neurons_mst(x = n, threshold = heal.threshold)) else n
}

#' @rdname neuprint_read_neurons
neuprint_read_neurons_simple <- function(bodyids, ...) {
  .Defunct("neuprint_read_skeletons")
}
