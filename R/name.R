#' @title Get the name of a neuron
#'
#' @description  If a bodyID has a name associated with it, fetch that name, otherwise, return NA
#' @inheritParams neuprint_get_adjacency_matrix
#' @return a vector of names. The vector is named with the given bodyids
#' @export
#' @rdname neuprint_get_names
neuprint_get_neuron_names <- function(bodyids, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  # Get a default dataset if none specified
  dataset <- check_dataset(dataset)
  conn=neuprint_login(conn)
  dp=neuprint_dataset_prefix(dataset, conn=conn)

  all_segments = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf("WITH %s AS bodyIds UNWIND bodyIds AS bodyId MATCH (n:`%s`) WHERE n.bodyId=bodyId RETURN n.instance AS name",
                   jsonlite::toJSON(unlist(bodyids)),
                   paste0(dp,all_segments))
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
  dataset <- check_dataset(dataset)
  all_segments = ifelse(all_segments,"Segment","Neuron")
  conn=neuprint_login(conn)
  cypher = sprintf(
    paste(
      "WITH %s AS bodyIds UNWIND bodyIds AS bodyId ",
      "MATCH (n:`%s_%s`) WHERE n.bodyId=bodyId",
      "RETURN n.bodyId AS bodyid, n.%s AS name, n.type AS type, n.status AS status, n.size AS voxels, n.pre AS pre, n.post AS post"
    ),
    jsonlite::toJSON(unlist(bodyids)),
    dataset,
    all_segments,
    neuprint_name_field(conn)
  )
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, include_headers = F, ...)
  neuprint_list2df(nc, return_empty_df = TRUE)
}

#' @title Get roiInfo associated with a body
#'
#' @description Return pre and post counts in all the ROIs given bodyids innervate.
#' @inheritParams neuprint_get_adjacency_matrix
#' @return a dataframe, one row for each given body id, columns ROI_pre and ROI_post for every ROI. If data is missing, NA is returned.
#' @export
#' @rdname neuprint_get_roiInfo
neuprint_get_roiInfo <- function(bodyids, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  dataset <- check_dataset(dataset)
  conn=neuprint_login(conn)
  dp=neuprint_dataset_prefix(dataset, conn=conn)
  all_segments = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf(
    "WITH %s AS bodyIds UNWIND bodyIds AS bodyId MATCH (n:`%s`) WHERE n.bodyId=bodyId RETURN n.bodyId AS bodyid, n.roiInfo AS roiInfo",
    jsonlite::toJSON(unlist(bodyids)),
    paste0(dp, all_segments)
  )
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, ...)
  lc <-  lapply(nc$data,function(x){cbind(bodyid=x[[1]],as.data.frame(t(unlist(jsonlite::fromJSON(x[[2]])))))})
  dfmerge <-  function(x) Reduce(function(...) merge(...,all.x=TRUE,all.y=TRUE),x)
  d <- dfmerge(lc)
  d
}


#' @title Search for body IDs based on a given name
#'
#' @description Search for bodyids corresponding to a given name, Reex sensitive
#' @inheritParams neuprint_get_adjacency_matrix
#' @param search name to search. Defaults to a search for MBONs
#' @param meta if TRUE, meta data for found bodyids is also pulled
#' @return a vector of body ids, or a data frame with their meta information
#' @export
#' @rdname neuprint_search
neuprint_search <- function(search = "MBON.*", meta = TRUE, all_segments = TRUE, dataset = NULL, conn = NULL, ...){
  dataset <- check_dataset(dataset)
  conn=neuprint_login(conn)
  dp=neuprint_dataset_prefix(dataset, conn=conn)
  all_segments.cypher = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf("MATCH (n:`%s`) WHERE n.%s=~'%s' RETURN n.bodyId",
                   paste0(dp, all_segments.cypher),
                   neuprint_name_field(conn),
                   search)
  nc = neuprint_fetch_custom(cypher=cypher, ...)
  bodyids=unlist(nc$data)
  if(meta){
    neuprint_get_meta(bodyids = bodyids, dataset = dataset, all_segments = all_segments, conn = conn, ...)
  } else {
    bodyids
  }
}
