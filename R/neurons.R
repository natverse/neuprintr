#' @title Read bodies from the neuPrint server
#'
#' @description  Get \code{nat::neuronlist} objects or data frames in the format
#'   of SWC files, for neurons retrievable from a neuPrint server
#' @param bodyids the body IDs for neurons/segments (bodies) you wish to query
#' @param nat whether or not to read neurons as \code{nat::neuronlist} objects
#'   (TRUE) or get SWC data frame (FALSE)
#' @param dataset optional, a dataset you want to query. If NULL, the default
#'   specified by your R environ file is used. See \code{neuprint_login} for
#'   details.
#' @param conn optional, a neuprintr connection object, which also specifies the
#'   neuPrint server see \code{?neuprint_login}. If NULL, your defaults set in
#'   your R.profile or R.environ are used.
#' @param ... methods passed to \code{neuprint_login}
#' @return a n x n matrix, where the rows are input neurons and the columns are
#'   their targets
#' @seealso \code{\link{neuprint_fetch_custom}}
#' @export
#' @rdname neuprint_read_neurons
neuprint_read_neurons <- function(bodyids, nat = TRUE, dataset = NULL, conn = NULL, ...){
  lapply(bodyids,function(bodyid) neuprint_read_neuron(bodyid=bodyid, dataset = dataset, conn= conn, ...))
}

# Hidden
neuprint_read_neuron <- function(bodyid, nat = TRUE, conn = NULL, ...){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  cypher = sprintf("MATCH (:`%s-Neuron` {bodyId:%s})-[:Contains]->(:Skeleton)-[:Contains]->(root :SkelNode) WHERE NOT (root)<-[:LinksTo]-() RETURN root.rowNumber AS rowId, root.location.x AS x, root.location.y AS y, root.location.z AS z, root.radius AS radius, -1 AS link ORDER BY root.rowNumber UNION match (:`%s-Neuron` {bodyId:%s})-[:Contains]->(:Skeleton)-[:Contains]->(s :SkelNode)<-[:LinksTo]-(ss :SkelNode) RETURN s.rowNumber AS PointNo, s.location.x AS X, s.location.y AS Y, s.location.z AS Z, s.radius AS radius, ss.rowNumber AS Parent ORDER BY s.rowNumber",
                   dataset,
                   bodyid,
                   dataset,
                   bodyid)
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, ...)
  d = data.frame(do.call(rbind,nc$data))
  colnames(d) = unlist(nc$columns)
  d
}
