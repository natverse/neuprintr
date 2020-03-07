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
  bodyids <- neuprint_ids(bodyids, dataset = dataset, conn = conn, unique = FALSE,mustWork = FALSE)
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
#' # or simpler
#' neuprint_get_meta('DA2')
#' }
neuprint_get_meta <- function(bodyids, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  conn = neuprint_login(conn)
  bodyids <- neuprint_ids(bodyids, conn=conn, dataset = dataset,unique=FALSE,mustWork = FALSE)
  all_segments = ifelse(all_segments,"Segment","Neuron")

  fieldNames <- neuprint_get_fields(possibleFields = c("bodyId","name","instance","type","status","statusLabel","pre","post","upstream","downstream","cropped",
                                                       "size","cellBodyFiber"),
                                    dataset=dataset,conn=conn,...)
  returnCypher <- paste0("n.",fieldNames," AS ",dfFields(fieldNames),collapse=" , ")
#n.bodyId AS bodyid, n.%s AS name, n.type AS type, n.status AS status, n.statusLabel AS statusLabel, n.size AS voxels, n.pre AS pre, n.post AS post,n.cropped AS cropped, exists(n.somaLocation) as soma, n.cellBodyFiber as cellBodyFiber, n.downstream as downstream"
  cypher = sprintf(
    paste(
      "WITH %s AS bodyIds UNWIND bodyIds AS bodyId ",
      "MATCH (n:`%s`) WHERE n.bodyId=bodyId",
      "RETURN %s"
    ),
    id2json(bodyids),
    all_segments,
    paste(returnCypher, ", exists(n.somaLocation) AS soma")
  )
  nc <- neuprint_fetch_custom(cypher=cypher, conn = conn, dataset = dataset, include_headers = FALSE, ...)
  meta <- neuprint_list2df(nc, return_empty_df = TRUE)
  meta <- meta[,names(meta) %in% c("bodyid","voxels","soma","name",neuprint_get_fields(conn=conn,dataset = dataset,...))]
  meta
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
#' @description \code{neuprint_search} searches for bodyids corresponding to a
#'   given name. By default performs regex sensitive matches against neuron
#'   \bold{name}s and returns a \code{data.frame}.
#' @inheritParams neuprint_get_adjacency_matrix
#' @param search Search query, by default a regular expression that must match
#'   the whole of the neuPrint instance name field. See examples and the
#'   \code{field}, \code{fixed} and \code{exact} for how this can be modified.
#' @param field the meta data field in which you want a match for your search
#'   query. Defaults to name (or instance, as handled by
#'   \code{neuprintr:::neuprint_name_field}). Other common options include type,
#'   status, cellBodyFiber etc.
#' @param fixed if \code{FALSE} (the default), \code{search} is interpreted as a
#'   regular expression (i.e. "Advanced input" in neuPrint Explorer). If
#'   \code{TRUE}, the string \code{search} is interpreted as a simple character
#'   string to be matched (the default search behavior in neuPrint explorer). In
#'   this case partial matches are fine.
#' @param exact Whether the query must match the whole field. This is always
#'   true for regular expression queries while the default (\code{NULL}) implies
#'   false for \code{fixed} queries.
#' @param meta if \code{TRUE}, meta data for found bodyids is also pulled
#' @return a vector of body ids, or a data frame with their meta information
#' @export
#' @rdname neuprint_search
#' @examples
#' \donttest{
#' neuprint_search(".*DA2.*")
#' neuprint_search(".*DA2.*", meta=FALSE)
#'
#' # Search the type field
#' neuprint_search("MBON.*", field = "type", meta=FALSE)
#' neuprint_search("MBON[0-9]+", field = "type", meta=FALSE)
#'
#' # compact specification of field
#' neuprint_search("type:MBON[0-9]+", meta=FALSE)
#'
#' # starts with MBON
#' neuprint_search("type:MBON.*", meta=FALSE)
#' }
#'
#' \dontrun{
#' neuprint_search("MBON.*", field = "type")
#'
#' # fixed=TRUE can be useful when you don't want to worry about special
#' # characters like brackets
#' neuprint_search("PEN_a(PEN1)", field="type", fixed=TRUE)
#' # by default fixed=TRUE returns partial matches
#' neuprint_search("MBON16", field = "type", fixed=TRUE)
#' # here the type must exactly match the query i.e. complete match
#' neuprint_search("MBON16", field = "type", fixed=TRUE, exact = TRUE)
#'
#' neuprint_search("AVF1", field = "cellBodyFiber")
#' neuprint_search("cellBodyFiber:AVF1")
#' }
#' @seealso \code{\link{neuprint_get_meta}},
#'   \code{\link{neuprint_get_neuron_names}}
neuprint_search <- function(search, field = "name", fixed=FALSE, exact=NULL,
                            meta = TRUE, all_segments = FALSE, dataset = NULL,
                            conn = NULL, ...){
  if(isTRUE(substr(search, 1, 1)=='/')) {
    fixed=FALSE
    search <- substr(search, 2, nchar(search))
  } else if(isTRUE(substr(search, 1, 1)=='!')) {
    exact <- fixed <- TRUE
    search <- substr(search, 2, nchar(search))
  }
  regexres <- stringr::str_match(search, "^([A-z]+):(.+)$")
  if(isFALSE(is.na(regexres[,2]))){
    field <- regexres[,2]
    search <- regexres[,3]
  }

  if(field=="name"){
    conn = neuprint_login(conn)
    field = neuprint_name_field(conn)
  }
  # we want fixed searches to be partial by default
  if(isTRUE(fixed) && is.null(exact))
    exact=FALSE
  if(isFALSE(fixed) && isFALSE(exact))
    warning("Ignoring exact=FALSE as regular expression searches are always exact!")
  all_segments.cypher = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf("MATCH (n:`%s`) WHERE n.%s %s \\\"%s\\\" RETURN n.bodyId",
                   all_segments.cypher,
                   field,
                   ifelse(fixed, ifelse(exact, "=", "CONTAINS"), "=~"),
                   search)
  nc = neuprint_fetch_custom(cypher=cypher, conn=conn, dataset = dataset, ...)
  foundbodyids=unlist(nc$data)
  if(meta && isTRUE(length(foundbodyids)>0)){
    neuprint_get_meta(bodyids = foundbodyids, dataset = dataset, all_segments = all_segments, conn = conn, ...)
  } else {
    foundbodyids
  }
}

#' @description \code{neuprint_ids} provides for flexible search / specification
#'   of neuprint body ids. Use it at the start of any function that accepts body
#'   ids. Queries are by default partial and fixed (i.e. non-regex) against
#'   type. Returns a character vector of bodyids.
#'
#' @param x A set of bodyids or a query
#' @param mustWork Whether to insist that at least one valid id is returned
#'   (default \code{TRUE})
#' @param unique Whether to ensure that only unique ids are returned (default
#'   \code{TRUE})
#' @param ... Additional arguments passed to \code{\link{neuprint_search}}
#' @inheritParams neuprint_search
#'
#' @return For \code{neuprint_ids}, a character vector of bodyids (of length 0
#'   when there are none and \code{mustWork=FALSE}).
#' @export
#' @seealso \code{\link[neuprintr]{neuprint_search}}
#' @section Query syntax: It is probably best just to look at the examples, but
#'   the query syntax is as follows where square brackets denote optional parts:
#'
#'   \code{[!/][<field>:]<query>}
#'
#'   Starting with the optional leading character. An exclamation mark denotes
#'   an exact, fixed search. The / denotes a regular expression (exact) search.
#'   When both are missing, a partial, fixed search is carried out.
#'
#'   The optional field argument terminated by a colon defines a field other
#'   than the default one to use for the query.
#'
#'   Finally the query itself is a plain text (fixed) or regular expression
#'   query.
#'
#' @examples
#' \donttest{
#' # exact match against whole type
#' neuprint_ids("!MBON01")
#' # partial match
#' neuprint_ids("MBON01")
#' # partial match against name field rather than type
#' neuprint_ids("name:MBON01")
#'
#' # initial / indicates to use regex search (which must be exact)
#' neuprint_ids("/MBON01")
#' # more interesting regex search
#' neuprint_ids("/MBON0[1-4]")
#'
#' # partial regex search against the name field (note leading/trailing .*)
#' neuprint_ids("/name:.*MBON0[1-4].*")
#' }
#' @rdname neuprint_search
#' @importFrom stats na.omit
neuprint_ids <- function(x, mustWork=TRUE, unique=TRUE, fixed=TRUE, conn=NULL, dataset=NULL, ...) {
  if(is.character(x) && length(x)==1 && !looks_like_bodyid(x)) {
    x <- neuprint_search(x, meta = F, field = 'type', fixed=fixed,
                         conn=conn, dataset=dataset, ...)
  }
  x <- id2char(x)
  if(isTRUE(mustWork) && isFALSE(length(na.omit(x))>0))
    stop("No valid ids provided!")
  if(isTRUE(unique)) unique(x) else x
}

#' @title Get available metadata fields for Neuron nodes
#' @return a vector of available fields
#' @param possibleFields : field names to choose from
#' @inheritParams neuprint_ROI_hierarchy
#' @export
#' @examples
#' \donttest{
#' neuprint_get_fields()
#' }
neuprint_get_fields <- function(possibleFields = c("bodyId", "pre", "post",
                                                   "upstream", "downstream",
                                                   "status", "statusLabel",
                                                   "cropped", "instance", "name",
                                                   "size", "type", "cellBodyFiber",
                                                   "somaLocation", "somaRadius"),
                                dataset = NULL, conn = NULL, ...){
  conn <- neuprint_login(conn)
  dataset <- check_dataset(dataset, conn=conn)
  cypher <- sprintf("MATCH (n :`Neuron`) UNWIND KEYS(n) AS k RETURN DISTINCT k AS neuron_fields LIMIT 20")
  fields <- unlist(neuprint_fetch_custom(cypher=cypher, conn=conn, dataset = dataset, ...)$data)
  return(fields[fields %in% possibleFields])
}

# Hidden. Neuprint to our fields translation
dfFields <- function(field_name) {
  transTable <- data.frame(
    neuprint = c(
      "bodyId",
      "pre",
      "post",
      "upstream",
      "downstream",
      "status",
      "statusLabel",
      "cropped",
      "instance",
      "name",
      "size",
      "type",
      "cellBodyFiber",
      "somaLocation",
      "somaRadius"
    ),
    rName = c(
      "bodyid",
      "pre",
      "post",
      "upstream",
      "downstream",
      "status",
      "statusLabel",
      "cropped",
      "name",
      "name",
      "voxels",
      "type",
      "cellBodyFiber",
      "somaLocation",
      "somaRadius"
    ),
    stringsAsFactors = FALSE
  )

  res=transTable$rName[match(field_name, transTable$neuprint)]
  res
}
