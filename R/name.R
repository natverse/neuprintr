#' @title Get the name of a neuron
#'
#' @description  If a bodyID has a name associated with it, fetch that name, otherwise, return NA
#' @inheritParams neuprint_get_adjacency_matrix
#' @return a dataframe givng a bodyid name correspondence
#' @seealso \code{\link{neuprint_search_name}}
#' @export
#' @rdname neuprint_get_names
neuprint_get_neuron_names <- function(bodyids, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  do.call(rbind,lapply(bodyids, neuprint_get_neuron_name, dataset = dataset, all_segments = all_segments, conn = conn, ...))
}

# hidden
neuprint_get_neuron_name <- function(bodyid, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  all_segments = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf("MATCH (n:`%s-%s`) WHERE n.bodyId IN %s RETURN n.name",
                   dataset,
                   all_segments,
                   jsonlite::toJSON(bodyid))
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, ...)
  name = unlist(ifelse(length(nc$data),nc$data,NA))
  df = data.frame(bodyid = bodyid,neuron_name=name)
  rownames(df) = bodyid
  df
}

neuprint_search_neuron_name <- function(search = "MBON.*", all_segments = TRUE){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  all_segments = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf('MATCH (n:`%s-%s`) WHERE n.name=~"%s" RETURN n.bodyId',
                   dataset,
                   all_segments,
                   search)
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, ...)
}
