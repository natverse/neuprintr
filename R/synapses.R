#' @title Get the locations of pre- and post synapses associated with a body
#'
#' @description Get the xyz locations, IDs and confidences of pre- and post
#'   synapses associated with a body
#' @inheritParams neuprint_read_neurons
#' @param dataset optional, a dataset you want to query. If NULL, the default
#'   specified by your R environ file is used. See \code{\link{neuprint_login}}
#'   for details.
#' @param roi a roi (i.e. neuropil volume) you want to query. Use
#'   \code{\link{neuprint_ROIs}} to see what is available. Defaults to 'all',
#'   which will return synapses in all ROIs.
#' @param progress if TRUE, a progress bar will be shown. This may slow the data
#'   fetching process.
#' @return a data frame, where each entry is a connection between the specified
#'   bodyid and its partner, either presynaptic to the bodyid (prepost=0) or
#'   postsynaptic (prepost=1). Each connection is associated with a synapse that
#'   has its own unique connector_id, XYZ coordinates and a confidence in its
#'   existence.
#' @seealso \code{\link{neuprint_fetch_custom}},
#'   \code{\link{neuprint_simple_connectivity}},
#'   \code{\link{neuprint_common_connectivity}}
#' @export
#' @examples
#' \donttest{
#' neuprint_get_synapses(c(818983130, 1796818119))
#' }
neuprint_get_synapses <- function(bodyids, roi = NULL, progress = FALSE, dataset = NULL, conn = NULL, ...){
  dataset <- check_dataset(dataset)
  conn = neuprint_login(conn)
  if (is.null(roi)) {
    roi = ""
  } else{
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
  if(progress){
    d  = do.call(rbind, pbapply::pblapply(bodyids,
                                          function(bi) tryCatch(neuprint_get_synapses(
      bodyids = as.numeric(bi),
      roi = roi,
      progress = FALSE,
      dataset = dataset,
      conn = conn, ...),
      error = function(e) NULL)))
    return(d)
  }
  dp=neuprint_dataset_prefix(dataset, conn=conn)
  prefixed_seg <- paste0(dp, "Segment")
  cypher.post = sprintf(paste("WITH %s AS bodyIds UNWIND bodyIds AS bodyId",
                              "MATCH (a:`%s`)-[:Contains]->(c:SynapseSet)-[:Contains]->(s:Synapse)<-[:SynapsesTo]-(:Synapse)<-[:Contains]-(:SynapseSet)<-[:Contains]-(b:`%s`)",
                              "WHERE a.bodyId=bodyId AND (s.type='post') %s",
                              "RETURN DISTINCT id(s) AS connector_id,",
                              "s.type AS prepost, s.location.x AS x ,s.location.y AS y, s.location.z AS z,",
                              "s.confidence AS confidence, a.bodyId AS bodyid, b.bodyId AS partner"),
                   jsonlite::toJSON(as.numeric(unlist(bodyids))),
                   prefixed_seg,
                   prefixed_seg,
                   roi)
  cypher.pre = sprintf(paste("WITH %s AS bodyIds UNWIND bodyIds AS bodyId",
                       "MATCH (a:`%s`)-[:Contains]->(c:SynapseSet)-[:Contains]->(s:Synapse)-[:SynapsesTo]->(:Synapse)<-[:Contains]-(:SynapseSet)<-[:Contains]-(b:`%s`)",
                       "WHERE a.bodyId=bodyId AND (s.type='pre') %s",
                       "RETURN DISTINCT id(s) AS connector_id,",
                       "s.type AS prepost, s.location.x AS x ,s.location.y AS y, s.location.z AS z,",
                       "s.confidence AS confidence, a.bodyId AS bodyid, b.bodyId AS partner"),
                        jsonlite::toJSON(as.numeric(unlist(bodyids))),
                        prefixed_seg,
                        prefixed_seg,
                        roi)
  nc.post = neuprint_fetch_custom(cypher=cypher.post, conn = conn, ...)
  nc.pre = neuprint_fetch_custom(cypher=cypher.pre, conn = conn, ...)
  m = rbind(do.call(rbind,nc.post$data),do.call(rbind,nc.pre$data))
  colnames(m) =  nc.post$columns
  m = data.frame(do.call(rbind,apply(m, 1, function(x) nullToNA(x))))
  m[,] = unlist(m)
  m$prepost = ifelse(m$prepost=="post",1,0)
 # m$bodyid = sapply(m$datasetBodyIds, function(i) unlist(strsplit(i,":"))[3])
 #  m$partner = sapply(m$datasetBodyIds, function(i) unlist(strsplit(i,":"))[2])
  m = m[,c("connector_id", "prepost", "x", "y", "z", "confidence", "bodyid", "partner")]
  m = subset(m, bodyid!=partner) # Automatically remove autapses, hopefully we only need to do this temporarily
  m
}
