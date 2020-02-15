#' @title Get the name of a neuron
#'
#' @description If a bodyid has a name associated with it, fetch that name, otherwise, return \code{NA}
#' @inheritParams neuprint_get_adjacency_matrix
#' @return a vector of names, named with the input bodyids
#' @export
#' @examples
#' \donttest{
#' neuprint_get_neuron_names(c(818983130, 1796818119))
#' }
neuprint_get_neuron_names <- function(bodyids, dataset = NULL, all_segments = TRUE, conn = NULL, ...) {
  all_segments.json = ifelse(all_segments,"Segment","Neuron")
  bodyids <- id2char(bodyids)
  if(any(duplicated(bodyids))) {
    ubodyids=unique(bodyids)
    unames=neuprint_get_neuron_names(bodyids=ubodyids, dataset=dataset,
                                     conn=conn, all_segments=all_segments, ...)
    res=unames[match(bodyids, ubodyids)]
    return(res)
  }

  cypher = sprintf("WITH %s AS bodyIds UNWIND bodyIds AS bodyId MATCH (n:`%s`) WHERE n.bodyId=bodyId RETURN n.instance AS name, n.bodyId AS bodyid",
                   id2json(bodyids),
                   all_segments.json)

  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, dataset = dataset, ...)
  df=neuprint_list2df(nc, return_empty_df = TRUE)
  nn=df$name
  names(nn) = df$bodyid
  missing=setdiff(bodyids, names(nn))
  if(length(missing)>0) {
    nn[missing]=NA_character_
    nn=nn[bodyids]
  }
  nn
}

#' @title Get key metadata for body (including name, type, status, size)
#' @details Sometimes a \code{cellBodyFiber} will be present even when the
#'   \code{soma} is not, so this may be a good test of if the neuron is present
#'   in the volume. The \code{cellBodyFiber} should be matched to (hemi)lineage
#'   information .
#' @return a \code{data.frame} containing the neuron's \itemize{ \item name
#'
#'   \item type Cell type of the neuron
#'
#'   \item status (Traced etc)
#'
#'   \item statusLabel similar to \code{status} but often a bit more specific

#'   \item size size in voxels
#'
#'   \item pre number of presynapses
#'
#'   \item post number of postsynapses
#'
#'   \item cropped whether the neuron is cropped by the hemibrain volume
#'
#'   \item soma whether the neuron has a soma in the hemibrain volume
#'
#'   \item cellBodyFiber names the tract connecting the soma to rest of neuron
#'
#'   }
#'
#' @inheritParams neuprint_get_adjacency_matrix
#' @export
#' @examples
#' \donttest{
#' da2s=neuprint_search(".*DA2.*")
#' neuprint_get_meta(da2s$bodyid)
#' }
neuprint_get_meta <- function(bodyids, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  conn = neuprint_login(conn)
  all_segments = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf(
    paste(
      "WITH %s AS bodyIds UNWIND bodyIds AS bodyId ",
      "MATCH (n:`%s`) WHERE n.bodyId=bodyId",
      "RETURN n.bodyId AS bodyid, n.%s AS name, n.type AS type, n.status AS status, n.statusLabel AS statusLabel, n.size AS voxels, n.pre AS pre, n.post AS post, n.cropped AS cropped, exists(n.somaLocation) as soma, n.cellBodyFiber as cellBodyFiber"
    ),
    id2json(bodyids),
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
neuprint_get_roiInfo <- function(bodyids, dataset = NULL, all_segments = FALSE, conn = NULL, ...){
  all_segments = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf(
    "WITH %s AS bodyIds UNWIND bodyIds AS bodyId MATCH (n:`%s`) WHERE n.bodyId=bodyId RETURN n.bodyId AS bodyid, n.roiInfo AS roiInfo",
    id2json(bodyids),
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
#' @param field the meta data field in which you want a match for your search query.
#' Defaults to name (or instance, as handled by \code{neuprintr:::neuprint_name_field}).
#' Other common options include type, status, cellBodyFiber etc.
#' @param fixed if FALSE (the default), \code{search} is interpreted as a regular expression
#' ("Advanced input" in neuprint explorer). If TRUE, the string \code{search} is interpreted as
#' a simple character string (the default search behavior in neuprint explorer) to be matched
#' (partial matches are fine)
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
#' neuprint_search("MBON.*",field = "type")
#' neuprint_search("AVF1",field = "cellBodyFiber")
#' neuprint_search("PEN_a(PEN1)",field="type",fixed=TRUE)
#' }
#' @seealso \code{\link{neuprint_get_meta}}, \code{\link{neuprint_get_neuron_names}}
neuprint_search <- function(search, field = "name", fixed=FALSE, meta = TRUE, all_segments = FALSE, dataset = NULL, conn = NULL, ...){
  if(field=="name"){
    conn = neuprint_login(conn)
    field = neuprint_name_field(conn)
  }
  all_segments.cypher = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf("MATCH (n:`%s`) WHERE n.%s %s '%s' RETURN n.bodyId",
                   all_segments.cypher,
                   field,
                   ifelse(fixed, "CONTAINS", "=~"),
                   search)
  nc = neuprint_fetch_custom(cypher=cypher, conn=conn, dataset = dataset, ...)
  foundbodyids=unlist(nc$data)
  if(meta && isTRUE(length(foundbodyids)>0)){
    neuprint_get_meta(bodyids = foundbodyids, dataset = dataset, all_segments = all_segments, conn = conn, ...)
  } else {
    foundbodyids
  }
}
