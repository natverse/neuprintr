#' @title Get the locations of pre- and post synapses associated with a body.
#'
#' @description Get the xyz locations, IDs and confidences of pre- and post
#'   synapses associated with a body. The \code{connector_id}
#' for each synapse is for the 'presynaptic' object. Separate IDs exist for the postsynapses in NeuPrint, and
#' these are not fetched b this function. This is similar to the logic use in \code{rcatmaid}.
#'
#' @inheritParams neuprint_read_neurons
#' @param dataset optional, a dataset you want to query. If NULL, the default
#'   specified by your R environ file is used. See \code{\link{neuprint_login}}
#'   for details.
#' @param roi a roi (i.e. neuropil volume) you want to query. Use
#'   \code{\link{neuprint_ROIs}} to see what is available. Defaults to 'all',
#'   which will return synapses in all ROIs.
#' @param remove.autapses Whether to remove autaptic connections (default TRUE)
#'   from the results. It appears that there is a relatively high number of
#'   false positive autapses.
#' @param chunk A logical specifying whether to split the query into multiple
#'   chunks or an integer specifiying the size of those chunks (which defaults
#'   to 20 when \code{chunk=TRUE}).
#' @param progress if TRUE, a progress bar will be shown. This may slow the data
#'   fetching process for smaller queries (<100 body ids). The default of
#'   \code{progress=NULL} will only show a progress bar if the query will be
#'   split into multiple chunks based on the \code{chunk} argument.
#' @return a data frame, where each entry is a connection between the specified
#'   bodyid and its partner, either presynaptic to the bodyid (prepost=0) or
#'   postsynaptic (prepost=1). Each connection is associated with a synapse that
#'   has its own unique connector_id, XYZ coordinates and a confidence in its
#'   existence.
#' @seealso \code{\link{neuprint_fetch_custom}},
#'   \code{\link{neuprint_simple_connectivity}},
#'   \code{\link{neuprint_common_connectivity}}
#' @export
#' @inheritParams neuprint_fetch_custom
#' @examples
#' \donttest{
#' neuprint_get_synapses(c(818983130, 1796818119))
#' }
neuprint_get_synapses <- function(bodyids, roi = NULL, remove.autapses=TRUE,
                                  chunk=TRUE, progress = NULL,
                                  dataset = NULL, conn = NULL, ...){
  conn = neuprint_login(conn)
  bodyids = neuprint_ids(bodyids, conn = conn, dataset = dataset)
  if (!is.null(roi)) {
    possible.rois = neuprint_ROIs(dataset = dataset, conn = conn, ...)
    if (sum(!roi %in% possible.rois) > 0) {
      stop(
        "Regions of interest provided  are not NULL or demarcated in dataset ",
        dataset,
        " for server ",
        neuprint_login(conn)$server,
        ". Please call neuprint_ROIs() to see the available ROIs."
      )
    }
    roi = sprintf("AND (exists(s.`%s`))", roi)
  }
  n <- length(bodyids)
  if(is.numeric(chunk)) {
    chunksize=chunk
  } else {
    # make smaller chunks when progress=T and there aren't so many bodyids
    if(isTRUE(progress))
      chunksize=min(20L, ceiling(n/10))
    else
      chunksize=20L
  }

  if(n>chunksize) {
    nchunks=ceiling(n/chunksize)
    chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(n)]
    bodyids <- split(bodyids, chunks)
    # if we got here and progess is unset then set it
    if(is.null(progress) || is.na(progress)) progress=TRUE
    MYPLY <- if(isTRUE(progress)) pbapply::pblapply else lapply
    d  = do.call(rbind, MYPLY(bodyids, function(bi) tryCatch(neuprint_get_synapses(
      bodyids = bi,
      roi = roi,
      progress = FALSE,
      dataset = dataset,
      conn = conn, ...),
      error = function(e) {warning(e); NULL})))
    # if you request multiple neurons in one go, they come back sorted by
    # prepost and then by body id. Not sure if that is the best order, but
    # probably best to emulate
    if(isTRUE(nrow(d)>1)) {
      d=d[order(d$prepost, decreasing = TRUE),]
      rownames(d)=NULL
    }
    return(d)
  }
  if(is.null(roi)) roi <- ""
  cypher.post = sprintf(paste("WITH %s AS bodyIds UNWIND bodyIds AS bodyId",
                              "MATCH (a:`%s`)-[:Contains]->(:SynapseSet)-[:Contains]->(p:Synapse)<-[:SynapsesTo]-(s:Synapse)<-[:Contains]-(c:SynapseSet)<-[:Contains]-(b:`%s`)",
                              "WHERE a.bodyId=bodyId AND (s.type='pre') %s",
                              "RETURN DISTINCT id(s) AS connector_id,",
                              "p.type AS prepost, p.location.x AS x ,p.location.y AS y, p.location.z AS z,",
                              "p.confidence AS confidence, a.bodyId AS bodyid, b.bodyId AS partner"),
                   id2json(bodyids),
                   "Segment",
                   "Segment",
                   roi)
  cypher.pre = sprintf(paste("WITH %s AS bodyIds UNWIND bodyIds AS bodyId",
                       "MATCH (a:`%s`)-[:Contains]->(c:SynapseSet)-[:Contains]->(s:Synapse)-[:SynapsesTo]->(:Synapse)<-[:Contains]-(:SynapseSet)<-[:Contains]-(b:`%s`)",
                       "WHERE a.bodyId=bodyId AND (s.type='pre') %s",
                       "RETURN DISTINCT id(s) AS connector_id,",
                       "s.type AS prepost, s.location.x AS x ,s.location.y AS y, s.location.z AS z,",
                       "s.confidence AS confidence, a.bodyId AS bodyid, b.bodyId AS partner"),
                        id2json(bodyids),
                       "Segment",
                       "Segment",
                        roi)
  nc.post = neuprint_fetch_custom(cypher=cypher.post, conn = conn, dataset = dataset)
  nc.pre = neuprint_fetch_custom(cypher=cypher.pre, conn = conn, dataset = dataset)
  m = rbind(neuprint_list2df(nc.post),neuprint_list2df(nc.pre))
  m$prepost = ifelse(m$prepost=="post",1,0)
  if(remove.autapses)   # Automatically remove autapses, hopefully we only need to do this temporarily
    m = subset(m, bodyid!=partner)
  m
}

# work in progress
neuprint_synapse_connections <- function(connector_ids){
  cypher = sprintf(paste("WITH %s AS connectorIds UNWIND connectorIds AS connectorId",
                             "MATCH (a:Segment)-[:Contains]->(:SynapseSet)-[:Contains]->(p:Synapse)<-[:SynapsesTo]-(s:Synapse)<-[:Contains]-(c:SynapseSet)<-[:Contains]-(b:Segment)",
                             "WHERE s.id=connectorId",
                             "RETURN id(s) AS connector_id,",
                             "s.type AS prepost, s.location.x AS x ,s.location.y AS y, s.location.z AS z,",
                             "s.confidence AS confidence, id(p) = p.connectorId"),
                       id2json(connector_ids))
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, dataset = dataset)
  m = rbind(neuprint_list2df(nc))
  m
}
