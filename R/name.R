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
  nn=as.character(df$name)
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
#' @return a \code{data.frame} containing the neuron's \itemize{
#'
#'   \item name
#'
#'   \item type Cell type of the neuron
#'
#'   \item status (Traced etc)
#'
#'   \item statusLabel similar to \code{status} but often a bit more specific
#'
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
#' @param possibleFields passed to \code{\link{neuprint_get_fields}} when not
#'   \code{NULL}, otherwise a default set are used.
#' @inheritParams neuprint_get_adjacency_matrix
#' @param chunk A logical specifying whether to split the query into multiple
#'   chunks or an integer specifying the size of those chunks (which defaults to
#'   2000 when \code{chunk=TRUE}).
#' @param progress default FALSE. If TRUE, the API is called separately for each
#'   neuron and you can assess its progress, if an error is thrown by any one
#'   \code{bodyid}, that \code{bodyid} is ignored.
#' @export
#' @examples
#' \donttest{
#' da2s=neuprint_search(".*DA2.*")
#' neuprint_get_meta(da2s$bodyid)
#' # or simpler
#' neuprint_get_meta('DA2')
#' }
#' \dontrun{
#' neuprint_get_meta('cropped:false')
#' }
neuprint_get_meta <- function(bodyids, dataset = NULL, all_segments = TRUE,
                              conn = NULL, chunk=TRUE, progress=FALSE,
                              possibleFields=NULL, ...){
  conn = neuprint_login(conn)
  dataset = check_dataset(dataset=dataset, conn=conn)

  bodyids <- neuprint_ids(bodyids, conn=conn, dataset = dataset, unique=FALSE, mustWork = FALSE)
  if(any(duplicated(bodyids))) {
    ubodyids=unique(bodyids)
    unames=neuprint_get_meta(bodyids=ubodyids, dataset=dataset,
                             conn=conn, all_segments=all_segments,
                             chunk=chunk, progress=progress, possibleFields=possibleFields, ...)
    res=unames[match(bodyids, ubodyids),]
    return(res)
  }


  nP <- length(bodyids)
  if(is.numeric(chunk)) {
    chunksize=chunk
  } else {
    # make smaller chunks when progress=T and there aren't so many bodyids
    if (chunk ==TRUE)
      if(isTRUE(progress))
        chunksize=min(2000L, ceiling(nP/10))
      else
        chunksize=2000L
      else
        chunksize=Inf
  }

  if(nP>chunksize) {
    nchunks=ceiling(nP/chunksize)
    chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nP)]
    bodyids <- split(bodyids, chunks)
    # if we got here and progress is unset then set it
    if(is.null(progress) || is.na(progress)) progress=TRUE
    MYPLY <- if(isTRUE(progress)) pbapply::pblapply else lapply
    ll=MYPLY(bodyids, function(bi) tryCatch(neuprint_get_meta(
      bodyids = bi,
      progress = FALSE,
      chunk=FALSE, # nb don't want to further chunk
      possibleFields=possibleFields,
      dataset = dataset, conn = conn, ...),
      error = function(e) {warning(e); NULL}))
    d  = try(dplyr::bind_rows(ll), silent = T)
    # dplyr has got fussier about type safety e.g. mixing char and int
    # this is a bit of a hack but easier than fussing about with type conversion
    if(inherits(d, 'try-error'))
      d <- do.call(rbind, ll)
    rownames(d) <- NULL
    return(d)
  }

  all_segments = ifelse(all_segments,"Segment","Neuron")

  if(is.null(possibleFields))
    possibleFields <- c("bodyId","name","instance","type","status",
                        "statusLabel","pre","post","upstream","downstream",
                        "cropped","size","cellBodyFiber","notes")

  fieldNames <- neuprint_get_fields(possibleFields = possibleFields,
                                    dataset=dataset,conn=conn)
  rfields=dfFields(fieldNames)
  returnCypher <- paste0("n.",fieldNames," AS ",rfields,collapse=" , ")
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
  meta <- neuprint_fix_column_types(meta, conn=conn, dataset=dataset)
  meta <- meta[,names(meta) %in% c(rfields, "soma")]
  meta
}

#' @title Get roiInfo associated with a body
#'
#' @description Return pre and post counts in all the ROIs given bodyids innervate.
#' @inheritParams neuprint_get_adjacency_matrix
#' @param chunk A logical specifying whether to split the query into multiple
#'   chunks or an integer specifying the size of those chunks (which defaults
#'   to 2000 when \code{chunk=TRUE}).
#' @param progress default FALSE. If TRUE, the API is called separately for
#' each neuron and you can assess its progress, if an error is thrown by any
#' one \code{bodyid}, that \code{bodyid} is ignored
#' @return a dataframe, one row for each given body id, columns ROI_pre and ROI_post for every ROI. If data is missing, NA is returned.
#' @export
#' @examples
#' \donttest{
#' neuprint_get_roiInfo(c(818983130, 1796818119))
#' }
neuprint_get_roiInfo <- function(bodyids, dataset = NULL, all_segments = FALSE, chunk=TRUE,progress=FALSE,conn = NULL, ...){
  if(any(duplicated(bodyids))) {
    ubodyids=unique(bodyids)
    unames=neuprint_get_roiInfo(bodyids=ubodyids, dataset=dataset,
                             conn=conn, all_segments=all_segments,chunk=chunk,progress=progress,...)
    res=unames[match(bodyids, ubodyids),]
    return(res)
  }

  nP <- length(bodyids)
  if(is.numeric(chunk)) {
    chunksize=chunk
  } else {
    # make smaller chunks when progress=T and there aren't so many bodyids
    if (chunk ==TRUE)
      if(isTRUE(progress))
        chunksize=min(2000L, ceiling(nP/10))
      else
        chunksize=2000L
      else
        chunksize=Inf
  }

  if(nP>chunksize) {
    nchunks=ceiling(nP/chunksize)
    chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nP)]
    bodyids <- split(bodyids, chunks)
    # if we got here and progess is unset then set it
    if(is.null(progress) || is.na(progress)) progress=TRUE
    MYPLY <- if(isTRUE(progress)) pbapply::pblapply else lapply
    d  = dplyr::bind_rows(MYPLY(bodyids, function(bi) tryCatch(neuprint_get_roiInfo(
      bodyids = bi,
      progress = FALSE,
      chunk = FALSE,
      dataset = dataset, conn = conn, ...),
      error = function(e) {warning(e); NULL})))
    rownames(d) <- NULL
    return(d)
  }


  all_segments = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf(
    "WITH %s AS bodyIds UNWIND bodyIds AS bodyId MATCH (n:`%s`) WHERE n.bodyId=bodyId RETURN n.bodyId AS bodyid, n.roiInfo AS roiInfo",
    id2json(bodyids),
    all_segments
  )
  nc = neuprint_fetch_custom(cypher=cypher, dataset = dataset, conn = conn, ...)
  lc <-  lapply(nc$data,function(x){c(list(bodyid=x[[1]]),unlist(jsonlite::parse_json(x[[2]])))})
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
#'
#' # full access to WHERE cypher queries over nodes (i.e. neurons) in neo4j
#' # NB fields must be prefixed with n. to indicate that they are node properties.
#' # note also that exists(n.somaLocation) is cypher to ensure soma==TRUE
#' neuprint_search("where:exists(n.somaLocation) AND n.post>30000 AND NOT n.cropped")
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

  if(!isTRUE(field=="where")){
    conn = neuprint_login(conn)
    if(field=="name")
      field = neuprint_name_field(conn, dataset = dataset)
  }
  # we want fixed searches to be partial by default
  if(isTRUE(fixed) && is.null(exact))
    exact=FALSE
  if(isFALSE(fixed) && isFALSE(exact))
    warning("Ignoring exact=FALSE as regular expression searches are always exact!")
  nodetype = ifelse(all_segments,'Segment','Neuron')

  if(isTRUE(tolower(field)=='where')) {
    # this is a raw CYPHER query
    where=search
  } else {
    fieldtype=neuprint_typeof(field, type = 'neo4j', conn=conn, dataset = dataset)
    if(fieldtype=="STRING") {
      search=glue('\\"{search}\\"')
      operator=ifelse(fixed, ifelse(exact, "=", "CONTAINS"), "=~")
    } else operator="="
    where=glue("n.{field} {operator} {search}")
  }

  cypher = glue("
                MATCH (n:`{nodetype}`) \\
                WHERE {where} \\
                RETURN n.bodyId")
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
#' @section Standard query syntax: It is probably best just to look at the
#'   examples, but the query syntax is as follows where square brackets denote
#'   optional parts:
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
#'
#' @section Extended query syntax: As a stepping stone to writing full CYPHER
#'   queries against Neo4J you can used the special \code{where} keyword to
#'   introduce your queries:
#'
#'   \code{where:<cypher query>}
#'
#'   e.g.
#'
#'   \code{"where:exists(n.somaLocation) AND n.post>30000 AND NOT n.cropped"}
#'
#'   Note that properties of individual nodes (i.e. neurons) must be prefixed
#'   with \code{n.} as would be typical in a CYPHER query. This feature is still
#'   experimental and details of the interface may still change. If you have
#'   feedback please do so at
#'   \url{https://github.com/natverse/neuprintr/pull/153}.
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
  if(is.character(x) && length(x)==1 && !looks_like_bodyid(x) && !is.na(x)) {
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
#' @param negateFields : Whether to include (FALSE, the default) or exclude \code{possibleFields}
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
                                                   "somaLocation", "somaRadius","notes"),
                                negateFields=FALSE,
                                dataset = NULL, conn = NULL, ...){

    cypher <- sprintf("MATCH (n :`Neuron`) UNWIND KEYS(n) AS k RETURN DISTINCT k AS neuron_fields")
    fields <- unlist(neuprint_fetch_custom(cypher=cypher, cache=TRUE, conn=conn, dataset = dataset, ...)$data)
    if (negateFields){fields <- fields[!(fields %in% possibleFields)]} else {fields <- fields[fields %in% possibleFields]}

  return(fields)
}

# Hidden. Neuprint to our fields translation
dfFields <- function(field_name) {
  transTable <- data.frame(
    neuprint = c(
      "bodyId",
      "instance",
      "size"
    ),
    rName = c(
      "bodyid",
      "name",
      "voxels"
    ),
    stringsAsFactors = FALSE
  )
  newnames=field_name
  tocheck=field_name %in% transTable$neuprint
  if(any(tocheck)) {
    newnames[tocheck]=transTable$rName[match(field_name[tocheck],
                                             transTable$neuprint)]
  }
  newnames
}

#' @importFrom glue glue
neuprint_typeof <- function(field, type=c("r", "neo4j"), cache=TRUE,
                            conn=NULL, dataset=NULL,  ...) {
  type=match.arg(type)
  if(length(field)>1) {
    ff=sapply(field, neuprint_typeof, type=type, cache=cache, conn=conn, dataset=dataset, ...)
    return(ff)
  }
  q <- if(type=='r') {"
    MATCH (n:Neuron)
    WHERE exists(n.`{field}`)
    RETURN n.{field} AS {field}
    LIMIT 1
  " } else {"
    MATCH (n:Neuron)
    WHERE exists(n.`{field}`)
    RETURN apoc.meta.type(n.`{field}`)
    LIMIT 1
  "}
  q=glue(gsub("\\s+", " ", q))
  r=try(neuprintr::neuprint_fetch_custom(q, include_headers = FALSE, cache = cache, conn=NULL, dataset=NULL, ...))
  if(inherits(r, 'try-error')) NA_character_
  else {
    urd=unlist(r$data, use.names = F)
    if(type=="r") ifelse(is.null(urd), NA_character_, mode(urd))
    else urd
  }
}


# Fix column types using neuprint_typeof information
neuprint_fix_column_types <- function(df, conn=NULL, dataset=NULL) {
  stopifnot(is.data.frame(df))
  ctypes=sapply(df, mode)
  for(cn in colnames(df)) {
    col=df[[cn]]
    # look for all NA logical columns
    if(!isTRUE(mode(col)=='logical')) next
    if(!all(is.na(col))) next
    newtype <- neuprint_typeof(cn, conn=conn, dataset = dataset, type = 'r')
    if(is.na(newtype) || newtype=='list') next
    # message(cn, ":", newtype)
    mode(df[[cn]])=newtype
  }
  df
}
