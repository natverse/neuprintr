#' @title Get the locations of pre- and post synapses associated with a body.
#'
#' @description Get the xyz locations, IDs and confidences of pre- and post
#'   synapses associated with a body. The \code{connector_id} for each synapse
#'   is for the 'presynaptic' object. Separate IDs exist for the postsynapses in
#'   neuPrint, and these are not fetched b this function. This is similar to the
#'   logic use in \code{rcatmaid}.
#'
#' @inheritParams neuprint_read_neurons
#' @param dataset optional, a dataset you want to query. If NULL, the default
#'   specified by your R environ file is used. See \code{\link{neuprint_login}}
#'   for details.
#' @param roi a ROI (i.e. neuropil volume) you want to query. Use
#'   \code{\link{neuprint_ROIs}} to see what is available. Defaults to 'all',
#'   which will return synapses in all ROIs.
#' @param remove.autapses Whether to remove autaptic connections (default TRUE)
#'   from the results. It appears that there is a relatively high number of
#'   false positive autapses.
#' @param chunk A logical specifying whether to split the query into multiple
#'   chunks or an integer specifying the size of those chunks (which defaults to
#'   20 when \code{chunk=TRUE}).
#' @param progress if TRUE, a progress bar will be shown. This may slow the data
#'   fetching process for smaller queries (<100 body ids). The default of
#'   \code{progress=NULL} will only show a progress bar if the query will be
#'   split into multiple chunks based on the \code{chunk} argument.
#' @return a data frame, where each row is a connection between one query
#'   \code{bodyid} and a single \code{partner} neuron. The \code{prepost} column
#'   should be interpreted as follows: \itemize{
#'
#'   \item{\code{prepost=0}}{ a downstream or \emph{output} partner,
#'   postsynaptic to the query neuron}
#'
#'   \item{\code{prepost=1}}{ an upstream or \emph{input} partner, presynaptic
#'   to the query neuron}
#'
#'   } Each connection is associated with a synapse that has its own unique
#'   \code{connector_id}, XYZ coordinates and a confidence in its existence.
#' @seealso \code{\link{neuprint_fetch_custom}},
#'   \code{\link{neuprint_simple_connectivity}},
#'   \code{\link{neuprint_common_connectivity}}
#' @export
#' @inheritParams neuprint_fetch_custom
#' @examples
#' \donttest{
#' syns=neuprint_get_synapses(c(818983130, 1796818119))
#' head(syns)
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
    roiQ = sprintf("AND (exists(s.`%s`))", roi)
  }
  n <- length(bodyids)
  if(is.numeric(chunk)) {
    chunksize=chunk
  } else {
    if (chunk ==TRUE)
      # make smaller chunks when progress=T and there aren't so many bodyids
      if(isTRUE(progress))
        chunksize=min(20L, ceiling(n/10))
      else
        chunksize=20L
    else
      chunksize=Inf
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
      chunk = FALSE,
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
  if(is.null(roi)) roiQ <- ""
  cypher.post = sprintf(paste("WITH %s AS bodyIds UNWIND bodyIds AS bodyId",
                              "MATCH (a:`%s`)-[:Contains]->(:SynapseSet)-[:Contains]->(p:Synapse)<-[:SynapsesTo]-(s:Synapse)<-[:Contains]-(c:SynapseSet)<-[:Contains]-(b:`%s`)",
                              "WHERE a.bodyId=bodyId AND (s.type='pre') %s",
                              "RETURN DISTINCT id(s) AS connector_id,",
                              "p.type AS prepost, p.location.x AS x ,p.location.y AS y, p.location.z AS z,",
                              "p.confidence AS confidence, a.bodyId AS bodyid, b.bodyId AS partner"),
                   id2json(bodyids),
                   "Segment",
                   "Segment",
                   roiQ)
  cypher.pre = sprintf(paste("WITH %s AS bodyIds UNWIND bodyIds AS bodyId",
                       "MATCH (a:`%s`)-[:Contains]->(c:SynapseSet)-[:Contains]->(s:Synapse)-[:SynapsesTo]->(:Synapse)<-[:Contains]-(:SynapseSet)<-[:Contains]-(b:`%s`)",
                       "WHERE a.bodyId=bodyId AND (s.type='pre') %s",
                       "RETURN DISTINCT id(s) AS connector_id,",
                       "s.type AS prepost, s.location.x AS x ,s.location.y AS y, s.location.z AS z,",
                       "s.confidence AS confidence, a.bodyId AS bodyid, b.bodyId AS partner"),
                        id2json(bodyids),
                       "Segment",
                       "Segment",
                        roiQ)
  nc.post = neuprint_fetch_custom(cypher=cypher.post, conn = conn, dataset = dataset, ...)
  nc.pre = neuprint_fetch_custom(cypher=cypher.pre, conn = conn, dataset = dataset, ...)
  m = rbind(neuprint_list2df(nc.post),neuprint_list2df(nc.pre))
  m$prepost = ifelse(m$prepost=="post",1,0)
  if(remove.autapses)   # Automatically remove autapses, hopefully we only need to do this temporarily
    m = subset(m, bodyid!=partner)
  m
}

#' @title Get the IDs and 3D locations of the pre- and post- synapses that make
#'   up a synaptic object
#'
#' @description Get synapse IDs for a given connector ID, as well as their
#'   associated body IDs and their locations in 3D space.
#'
#' @inheritParams neuprint_read_neurons
#' @inheritParams neuprint_fetch_custom
#' @param connector_ids a vector of IDs (pre- or postsynapse IDs) for a synaptic
#'   connection object.
#' @param ... Additional arguments passed to \code{\link{neuprint_fetch_custom}}
#'
#' @return a data frame, where each entry gives the pre-post associations for a
#'   synapse. This includes which are the pre/post synaptic bodyids
#'   (neurons/fragments) and the location of these synapses in 3D space (likely
#'   raw voxel space).
#' @seealso \code{\link{neuprint_fetch_custom}},
#'   \code{\link{neuprint_get_synapses}}
#' @export
#' @examples
#' \donttest{
#' syns = neuprint_get_synapses(818983130) # note
#' # all given connectorIDs from this function are for presynapses
#' prepost.conn = neuprint_connectors(syns$connector_id, all_segments = TRUE)
#' head(prepost.conn) # All the synapses that connect to or from 818983130
#' # and their other connections to other bodies too
#' }
neuprint_connectors <- function(connector_ids, all_segments = TRUE,
                                conn=NULL, dataset=NULL, ...){
  all_segments_json = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf(paste("WITH %s AS connectorIds",
                           "UNWIND connectorIds AS connectorId",
                           "MATCH (ms:Synapse)",
                           "WHERE id(ms) = connectorId",
                           "MATCH (ms:Synapse)-[:SynapsesTo]-(ns),",
                           "(mss:SynapseSet)-[:Contains]->(ms),",
                           "(nss:SynapseSet)-[:Contains]->(ns),",
                           "(n:%s)-[:Contains]->(nss),",
                           "(m:%s)-[:Contains]->(mss)",
                           "RETURN DISTINCT id(ns) as connectorId_1,",
                           "ns.type as prepost_1, id(ms) as connectorId_2, ms.type as prepost_2, n.bodyId as bodyId_1,",
                           "m.bodyId as bodyId_2, ns.location.x AS x_1 ,ns.location.y AS y_1, ns.location.z AS z_1,",
                           "ns.confidence AS confidence_1, ms.location.x AS x_2 ,ms.location.y AS y_2, ms.location.z AS z_2,",
                           "ms.confidence AS confidence_2"),
                     id2json(connector_ids),
                     all_segments_json,
                     all_segments_json)
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, dataset = dataset, ...)
  m = rbind(neuprint_list2df(nc))
  m1 = m[m$prepost_1=="pre",]
  m2 = m[m$prepost_1=="post",]
  colnames(m1) = gsub("_1","_pre",colnames(m1))
  colnames(m1) = gsub("_2","_post",colnames(m1))
  colnames(m2) = gsub("_1","_post",colnames(m2))
  colnames(m2) = gsub("_2","_pre",colnames(m2))
  m = rbind(m1, m2)
  m = m[, !grepl("prepost",colnames(m))]
  m
}

### Downlod all synapses:
# cypher = " MATCH (n:Neuron)-[e:ConnectsTo]->(m:Neuron),
#        (n)-[:Contains]->(nss:SynapseSet),
#        (m)-[:Contains]->(mss:SynapseSet),
#        (nss)-[:ConnectsTo]->(mss),
#        (nss)-[:Contains]->(ns:Synapse),
#        (mss)-[:Contains]->(ms:Synapse),
#        (ns)-[:SynapsesTo]->(ms)
#  // Artificial break in the query flow to fool the query
#  // planner into avoiding a Cartesian product.
#  // This improves performance considerably in some cases.
#  WITH n,e,m,ns,ms,true as _
#  WITH n, m, ns, ms, e
#  WHERE e.weight >= 1
#  WITH n, m, ns, ms
#  WHERE (ns.type = 'pre')
#  WITH n, m, ns, ms
#  WHERE (ms.type = 'post')
#  RETURN n.bodyId as bodyId_pre,
#         m.bodyId as bodyId_post,
#         id(ns) as connectorid_pre,
#         ns.location.x as ux,
#         ns.location.y as uy,
#         ns.location.z as uz,
#         id(ms) as connectorid_post,
#         ms.location.x as dx,
#         ms.location.y as dy,
#         ms.location.z as dz,
#         ns.confidence as confidence_pre,
#         ms.confidence as confidence_post
# SKIP 100
# LIMIT 10"
