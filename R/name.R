#' @title Get the name of a neuron
#'
#' @description  If a bodyID has a name associated with it, fetch that name, otherwise, return NA
#' @inheritParams neuprint_get_adjacency_matrix
#' @return a vector of names, named with the input bodyids
#' @export
#' @examples
#' \donttest{
#' neuprint_get_neuron_names(c(818983130, 1796818119))
#' }
neuprint_get_neuron_names <- function(bodyids, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  conn=neuprint_login(conn)

  all_segments.json = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf("WITH %s AS bodyIds UNWIND bodyIds AS bodyId MATCH (n:`%s`) WHERE n.bodyId=bodyId RETURN n.instance AS name",
                   jsonlite::toJSON(as.numeric(unlist(bodyids))),
                   all_segments.json)

  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, dataset = dataset, ...)
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
#' @examples
#' \donttest{
#' neuprint_get_meta(c(818983130, 1796818119))
#' }
neuprint_get_meta <- function(bodyids, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  all_segments = ifelse(all_segments,"Segment","Neuron")
  conn=neuprint_login(conn)
  cypher = sprintf(
    paste(
      "WITH %s AS bodyIds UNWIND bodyIds AS bodyId ",
      "MATCH (n:`%s`) WHERE n.bodyId=bodyId",
      "RETURN n.bodyId AS bodyid, n.%s AS name, n.type AS type, n.status AS status, n.size AS voxels, n.pre AS pre, n.post AS post, n.cropped AS cropped"
    ),
    jsonlite::toJSON(as.numeric(unlist(bodyids))),
    all_segments,
    neuprint_name_field(conn)
  )
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, dataset = dataset, include_headers = FALSE, ...)
  neuprint_list2df(nc, return_empty_df = TRUE)
}

#' @title Get roiInfo associated with a body
#'
#' @description Return pre and post counts in all the ROIs given bodyids innervate.
#' @inheritParams neuprint_get_adjacency_matrix
#' @return a dataframe, one row for each given body id, columns ROI_pre and ROI_post for every ROI. If data is missing, NA is returned.
#' @export
#' @examples
#' \donttest{
#' neuprint_get_roiInfo(c(818983130, 1796818119))
#' }
neuprint_get_roiInfo <- function(bodyids, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  conn=neuprint_login(conn)
  all_segments = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf(
    "WITH %s AS bodyIds UNWIND bodyIds AS bodyId MATCH (n:`%s`) WHERE n.bodyId=bodyId RETURN n.bodyId AS bodyid, n.roiInfo AS roiInfo",
    jsonlite::toJSON(as.numeric(unlist(bodyids))),
    all_segments
  )
  nc = neuprint_fetch_custom(cypher=cypher, dataset = dataset, conn = conn, ...)
  lc <-  lapply(nc$data,function(x){cbind(bodyid=x[[1]],as.data.frame(t(unlist(jsonlite::fromJSON(x[[2]])))))})
  d <- dplyr::bind_rows(lc)
  d
}


#' @title Search for body IDs based on a given name
#'
#' @description Search for bodyids corresponding to a given name, Regex sensitive
#' @inheritParams neuprint_get_adjacency_matrix
#' @param search name to search. See examples.
#' @param meta if TRUE, meta data for found bodyids is also pulled
#' @return a vector of body ids, or a data frame with their meta information
#' @export
#' @rdname neuprint_search
#' @examples
#' \donttest{
#' neuprint_search(".*DA2.*")
#' }
#' \dontrun{
#' neuprint_search("MBON.*")
#' }
neuprint_search <- function(search, meta = TRUE, all_segments = TRUE, dataset = NULL, conn = NULL, ...){
  conn=neuprint_login(conn)
 # dp=neuprint_dataset_prefix(dataset, conn=conn)
  all_segments.cypher = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf("MATCH (n:`%s`) WHERE n.%s=~'%s' RETURN n.bodyId",
                   all_segments.cypher,
                   neuprint_name_field(conn),
                   search)
  nc = neuprint_fetch_custom(cypher=cypher, dataset = dataset, ...)
  foundbodyids=unlist(nc$data)
  if(meta && isTRUE(length(foundbodyids)>0)){
    neuprint_get_meta(bodyids = foundbodyids, dataset = dataset, all_segments = all_segments, conn = conn, ...)
  } else {
    foundbodyids
  }
}
