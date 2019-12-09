# hidden
neuprint_fetch <- function(path, body = NULL, conn = NULL, parse.json = TRUE,
                           include_headers = TRUE, simplifyVector = FALSE, ...){
  path = gsub("\\/$|^\\/","",path)
  conn = neuprint_login(conn)
  req <-
    if (is.null(body)) {
      httr::GET(url = file.path(conn$server, path, fsep = "/"),
                config = conn$config,  ...)
    }else {
      httr::POST(url = file.path(conn$server, path, fsep = "/"),
           body = body, config = conn$config, ...)
    }
  httr::stop_for_status(req)
  if (parse.json) {
    parsed = neuprint_parse_json(req, simplifyVector = simplifyVector)
    if (length(parsed) == 2 && isTRUE(names(parsed)[2] =="error")) {
      stop("neuprint error: ", parsed$error)
    }
    if (include_headers) {
      fields_to_include = c("url", "headers")
      attributes(parsed) = c(attributes(parsed), req[fields_to_include])
    }
    parsed
  }
  else req
}

# hidden
neuprint_parse_json <- function (req, simplifyVector = FALSE, ...) {
  text <- httr::content(req, as = "text", encoding = "UTF-8")
  if (identical(text, ""))
    stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = simplifyVector, ...)
}

#' Parse neuprint return list to a data frame
#'
#' @details A low level function tailored to the standard neuprint list return
#'   format. Should handle those times when jsonlite's simplifcation doesn't
#'   work. The normal return value of \code{\link{neuprint_fetch}} is a list
#'   formatted as follows: \itemize{
#'
#'   \item{columns}{ List of column names}
#'
#'   \item{data}{ Nested list of data, with each row formatted as a single
#'   sublist}
#'
#'   \item{debug }{ Character vector containing query}}
#'
#'   If \code{neuprint_list2df} receives such a list it will use the
#'   \code{columns} to define the names for a data.frame constructed from the
#'   \code{data} field.
#'
#' @param x A list returned by \code{\link{neuprint_fetch}}
#' @param cols Character vector specifying which columns to include (by default
#'   all of those named in \code{x}, see details).
#' @param return_empty_df Return a zero row data frame when there is no result.
#' @param ... Additional arguments passed to \code{\link{as.data.frame}}
#' @export
neuprint_list2df <- function(x, cols=NULL, return_empty_df=FALSE, ...) {

  if(!length(x)) {
    return(if(return_empty_df){
      as.data.frame(structure(replicate(length(cols), logical(0)), .Names=cols))
    } else NULL)
  }

  if(length(x)>=2 && all(c("columns", "data") %in% names(x))) {
    if(is.null(cols)) cols=unlist(x$columns)
    x=x$data
  }

  if(is.character(x))

  l=list()
  for(i in seq_along(cols)) {
    colidx=if(use.col.names) cols[i] else i
    raw_col = sapply(x, "[[", colidx)
    if(is.list(raw_col)) {
      raw_col[sapply(raw_col, is.null)]=NA
      sublens=sapply(raw_col, length)
      if(all(sublens==1))
        raw_col=unlist(raw_col)
      else raw_col=sapply(raw_col, paste, collapse=',')
    }
    l[[cols[i]]]=raw_col
  }
  as.data.frame(l, ...)
}

#' @importFrom memoise memoise
neuprint_name_field <- memoise(function(conn=NULL) {
  if (is.null(conn))
    stop(
      "You must do\n  conn = neuprint_login(conn)\n",
      "before using neuprint_name_field(conn) in your function!",
      call. = FALSE
    )
  q="MATCH (n :hemibrain_Neuron) WHERE exists(n.instance) RETURN count(n)"
  neuprint_fetch_custom(q, conn = conn)
  n=unlist(neuprint_fetch_custom(q, include_headers=F)[['data']])
  return(ifelse(n>0, "instance", "name"))
})
