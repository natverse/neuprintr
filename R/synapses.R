#' @title Get the locations of pre- and post synapses associated with a body
#'
#' @description  Get the xyz locations, IDs and confidences of pre- and post synapses associated with a body
#' @inheritParams neuprint_read_neurons
#' @param dataset optional, a dataset you want to query. If NULL, the default specified by your R environ file is used. See \code{neuprint_login} for details.
#' @param roi a roi (i.e. neuropil volume) you want to query. Use \code{neuprint_ROIs} to see what is available.
#' Defaults to 'all', which will return synapses in all ROIs.
#' @return a data frame, where each entry is a connection between the specified bodyid and its partner, either presynaptic to the bodyid (prepost=0) or postsynaptic (prepost=1).
#' Each connection is associated with a synapse that has its own unique connector_id, XYZ coordinates and a confidence in its existence.
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_simple_connectivity}}, \code{\link{neuprint_common_connectivity}}
#' @export
#' @rdname neuprint_get_synapses
neuprint_get_synapses <- function(bodyids, roi = NULL, progress = FALSE, dataset = NULL, conn = NULL, ...){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  if(is.null(roi)){
    roi = ""
  }else{
    possible.rois = neuprint_ROIs(dataset=dataset,conn=conn, ...)
    if(sum(!roi%in%possible.rois)>0){
      stop("Regions of interest provided  are not NULL or demarcated in dataset ", dataset, " for server ", neuprint_login(conn)$server,
           ". Please call neuprint_ROIs() to see the available ROIs.")
    }
    roi = sprintf("AND (exists(s.%s))", roi)
  }
  if(progress){
    d  = do.call(rbind, pbapply::pblapply(bodyids, function(bi) tryCatch(neuprint_get_synapses(
      bodyids = bi,
      roi = roi,
      progress = FALSE,
      dataset = dataset,
      conn = conn, ...),
      error = function(e) NULL)))
    return(d)
  }
  cypher = sprintf("WITH %s AS bodyIds UNWIND bodyIds AS bodyId MATCH (a:`%s-Segment`)<-[:To]-(c:ConnectionSet), (c)-[:Contains]->(s:Synapse) WHERE a.bodyId=bodyId %s RETURN id(s) AS connector_id, s.type AS prepost, s.location.x AS x ,s.location.y AS y, s.location.z AS z, s.confidence AS confidence, c.datasetBodyIds AS datasetBodyIds, c.timeStamp AS timestamp",
                   jsonlite::toJSON(unlist(bodyids)),
                   dataset,
                   roi)
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, ...)
  m = do.call(rbind,nc$data)
  colnames(m) =  nc$columns
  m = as.data.frame(m)
  m$prepost = ifelse(m$prepost=="post",1,0)
  m$bodyid = sapply(m$datasetBodyIds, function(i) unlist(strsplit(i,":"))[3])
  m$partner = sapply(m$datasetBodyIds, function(i) unlist(strsplit(i,":"))[2])
  m = m[,c("connector_id", "prepost", "x", "y", "z", "confidence", "bodyid", "partner", "timestamp")]
  m
}
