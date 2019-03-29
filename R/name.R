#' @title Get the name of a neuron
#'
#' @description  If a bodyID has a name associated with it, fetch that name, otherwise, return NA
#' @inheritParams neuprint_get_adjacency_matrix
#' @return a vector of names. The vector is named with the given bodyids
#' @export
#' @rdname neuprint_get_names
neuprint_get_neuron_names <- function(bodyids, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  all_segments = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf("WITH %s AS bodyIds UNWIND bodyIds AS bodyId MATCH (n:`%s-%s`) WHERE n.bodyId=bodyId RETURN n.name AS name",
                   jsonlite::toJSON(unlist(bodyids)),
                   dataset,
                   all_segments)
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, ...)
  d =  unlist(lapply(nc$data,nullToNA))
  names(d) = bodyids
  d
}

#' @title Get meta data associated with a body
#'
#' @description Return important meta data given bodyids, including the putative neuron's name, status, size in voxels, and number of pre and post synapses.
#' @inheritParams neuprint_get_adjacency_matrix
#' @return a dataframe, one row for each given body id, columns bodyid, name, status, voxels, pre and post. If data is missing, NA is returned.
#' @export
#' @rdname neuprint_get_meta
neuprint_get_meta <- function(bodyids, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  all_segments = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf("WITH %s AS bodyIds UNWIND bodyIds AS bodyId MATCH (n:`%s-%s`) WHERE n.bodyId=bodyId RETURN n.bodyId AS bodyid, n.name AS name, n.status AS status, n.size AS voxels, pre, post",
                   jsonlite::toJSON(unlist(bodyids)),
                   dataset,
                   all_segments)
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, ...)
  d =  as.data.frame(do.call(rbind,lapply(nc$data,nullToNA)))
  colnames(d) = unlist(nc$columns)
  d
}


neuprint_search_neuron_name <- function(search = "MBON.*", all_segments = TRUE, ...){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  all_segments = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf('MATCH (n:`%s-%s`) WHERE n.name=~"%s" RETURN n.bodyId',
                   dataset,
                   all_segments,
                   search)
  nc = neuprint_fetch_custom(cypher=cypher, ...)
}
