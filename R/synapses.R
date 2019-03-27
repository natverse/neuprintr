#' @title Get the locations of pre- and post synapses associated with a body
#'
#' @description  Get the xyz locations, IDs and confidences of pre- and post synapses associated with a body
#' @param bodyid the body ID for a neuron/segment (body) you wish to query
#' @param dataset optional, a dataset you want to query. If NULL, the default specified by your R environ file is used. See \code{neuprint_login} for details.
#' @param roi a roi (i.e. neuropil volume) you want to query. Use \code{neuprint_ROIs} to see what is available.
#' Defaults to 'all', which will return synapses in all ROIs.
#' @param conn optional, a neuprintr connection object, which also specifies the neuPrint server see \code{?neuprint_login}.
#' If NULL, your defaults set in your R.profile or R.environ are used.
#' @param ... methods passed to \code{neuprint_login}
#' @return a n x n matrix, where the rows are input neurons and the columns are their targets
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_simple_connectivity}}, \code{\link{neuprint_common_connectivity}}
#' @export
#' @rdname neuprint_get_synapses
neuprint_get_synapses <- function(bodyid = bodyid, dataset = NULL, roi = "all", conn = NULL, ...){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  if(roi=="all"){
    roi = ""
  }else{
    possible.rois = neuprint_ROIs(dataset=dataset,conn=conn, ...)
    if(sum(!roi%in%possible.rois)>0){
      stop("Regions of interest provided  are not 'all' or demarcated in dataset ", dataset, " for server ", neuprint_login(conn)$server,
           ". Please call neuprint_ROIs() to see the available ROIs.")
    }
    roi = sprintf("WHERE (exists(s.%s))", roi)
  }
  cypher = sprintf("MATCH (a:`%s-Neuron`{bodyId:%s})<-[:To]-(c:ConnectionSet), (c)-[:Contains]->(s:Synapse) %s RETURN id(s) AS connector_id, s.type AS prepost, s.location.x AS x ,s.location.y AS y, s.location.z AS z, s.confidence AS confidence",
                   dataset,
                   bodyid,
                   roi)
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, ...)
  m = do.call(rbind,nc$data)
  colnames(m) =  nc$columns
  m = as.data.frame(m)
  m$prepost = ifelse(m$prepost=="post",1,0)
  m$bodyid = bodyid
  m
}
