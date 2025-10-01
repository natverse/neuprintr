# hidden
neuprint_fetch <- function(path,
                           body = NULL,
                           conn = NULL,
                           parse.json = TRUE,
                           include_headers = TRUE,
                           simplifyVector = FALSE,
                           app = NULL,
                           cache = FALSE,
                           ...) {
  path = gsub("\\/$|^\\/", "", path)
  conn = neuprint_login(conn)
  server = sub("\\/$", "", conn$server) # you cannot have double / in any part of path
  conf = conn$config
  if (cache)
    neuprint_fetch_impl_memo(
      path,
      body = body,
      server = server,
      conf = conf,
      parse.json = parse.json,
      include_headers = include_headers,
      simplifyVector = simplifyVector,
      app = app,
      ...
    )
  else
    neuprint_fetch_impl(
      path,
      body = body,
      server = server,
      conf = conf,
      parse.json = parse.json,
      include_headers = include_headers,
      simplifyVector = simplifyVector,
      app = app,
      ...
    )
}

neuprint_fetch_impl <- function(path, body = NULL, server = NULL, conf = NULL,
                               parse.json = TRUE, include_headers = TRUE,
                               simplifyVector = FALSE, app=NULL, ...) {
  if(is.null(app))
    app=paste0("neuprintr/", utils::packageVersion('neuprintr'))
  req <-
    if (is.null(body)) {
      httr::GET(url = file.path(server, path, fsep = "/"),
                config = conf, httr::user_agent(app), ...)
    } else {
      httr::POST(url = file.path(server, path, fsep = "/"),
                 body = body, config = conf, httr::user_agent(app), ...)
    }
  neuprint_error_check(req)
  if (parse.json) {
    parsed = neuprint_parse_json(req, simplifyVector = simplifyVector)
    # Has a return value like this ever been seen in the wild?
    if (length(parsed) == 2 && isTRUE(names(parsed)[2] =="error")) {
      stop("neuPrint error: ", parsed$error)
    }
    if (include_headers) {
      fields_to_include = c("url", "headers")
      attributes(parsed) = c(attributes(parsed), req[fields_to_include])
    }
    parsed
  }
  else req
}


neuprint_fetch_impl_memo <- memoise::memoise(neuprint_fetch_impl, ~memoise::timeout(3600))

# hidden
neuprint_parse_json <- function (req, simplifyVector = FALSE, ...) {
  text <- httr::content(req, as = "text", encoding = "UTF-8")
  if (identical(text, ""))
    stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = simplifyVector, ...)
}

# hidden
neuprint_error_check <- function(req) {
  sc=httr::status_code(req)
  if(sc<300) return(req)

  task=paste("process url:", req$url)
  if(isTRUE(sc %in% c(400L, 500L))) {
    parsed=neuprint_parse_json(req)
    msg=as.character(unlist(parsed))
    task=paste(task, "with neuPrint error:", msg)
  }
  call <- sys.call(-1)
  stop(httr::http_condition(req, "error",
                            task =task,
                            call = call))
}

#' Parse neuprint return list to a data frame
#'
#' @details A low level function tailored to the standard neuprint list return
#'   format. Should handle those times when jsonlite's simplification doesn't
#'   work. The normal return value of \code{\link{neuprint_fetch_custom}} is a
#'   list formatted as follows: \describe{
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
#' @param x A list returned by \code{\link{neuprint_fetch_custom}}
#' @param cols Character vector specifying which columns to include (by default
#'   all of those named in \code{x}, see details).
#' @param return_empty_df Return a zero row data frame when there is no result.
#' @param stringsAsFactors Whether to return character vector columns as
#'   factors. Note that the default of \code{FALSE} differs from
#'   \code{\link{data.frame}} and friends.
#' @param check.names Whether to convert column names into R friendly form. This
#'   is not necessary but would be the default for \code{\link{data.frame}} were
#'   we not to set it ourselves. See \code{\link{as.data.frame}} for details.
#' @param ... Additional arguments passed to \code{\link{as.data.frame}}
#'
#' @export
neuprint_list2df <- function(x, cols=NULL, return_empty_df=FALSE,
                             check.names=FALSE,
                             stringsAsFactors=FALSE, ...) {

  if(length(x)>=2 && all(c("columns", "data") %in% names(x))) {
    if(is.null(cols)) cols=unlist(x$columns)
    x=x$data
    colidxs=match(cols, cols)
  } else {
    colidxs=seq_along(cols)
  }

  if(!length(x)) {
    return(if(return_empty_df){
      as.data.frame(structure(replicate(length(cols), logical(0)), .Names=cols), check.names=check.names)
    } else NULL)
  }

  l=list()
  for(i in seq_along(cols)) {
    colidx=colidxs[i]
    firstrec=x[[1]][[colidx]]
    raw_col=lapply(x, "[[", colidx)
    if(any(lengths(raw_col)==3) && any(grepl("coordinates", sapply(raw_col, names)))) {
      # special case coordinates
      raw_col <- lapply(raw_col, simplify_coords)
    }
    sublens=lengths(raw_col)
    if(all(sublens%in% 0:1)) {
      if(any(sublens==0))
        raw_col[sublens==0]=NA
      raw_col=unlist(raw_col)
    } else raw_col=sapply(raw_col, paste, collapse=',')
    l[[cols[i]]]=raw_col
  }
  as.data.frame(l, stringsAsFactors=stringsAsFactors, check.names=check.names, ...)
}

simplify_coords <- function(r) {
  if(length(r)==0) NULL
  else if('coordinates' %in% names(r))
    r[['coordinates']]
  else if(any(grepl("{", fixed = T, r)))
    c(nat::xyzmatrix(r))
  else NULL
}

#' @importFrom memoise memoise
neuprint_name_field <- memoise(function(conn=NULL, dataset = NULL) {
  if (is.null(conn))
    stop(
      "You must do\n  conn = neuprint_login(conn)\n",
      "before using neuprint_name_field(conn) in your function!",
      call. = FALSE
    )
  q = "MATCH (n :Neuron) WHERE exists(n.instance) RETURN count(n)"
  n=unlist(neuprint_fetch_custom(q, conn = conn, dataset = dataset,
                                 include_headers=F)[['data']])
  return(ifelse(n>0, "instance", "name"))
})

# hidden
neuprint_dataset_prefix <- memoise(function(dataset, conn=NULL) {
  if (is.null(conn))
    stop(
      "You must do\n  conn = neuprint_login(conn)\n",
      "before using neuprint_dataset_prefix(conn) in your function!",
      call. = FALSE
    )
  # q=sprintf("MATCH (n:`%s_Segment`) RETURN count(n)", dataset)
  # n=unlist(neuprint_fetch_custom(q, dataset = dataset, include_headers=F)[['data']])
  #paste0(dataset, ifelse(n>0, "_", "-")) # I think we no longer need to specify the dataset. Might be good to keep this function in place though, in case situation changes
  ""
})

# hidden
#' @importFrom httr parse_url
check_dataset <-
  function(dataset = NULL, conn = NULL) {
    # login if NULL
    conn=neuprint_login(conn)

    # use the default in the connection object if no argument specified
    if(is.null(dataset)) dataset=conn$dataset

    # see what datasets are available for this connection
    datasets4conn <- available_datasets(conn)

    if (is.null(dataset)) {
      # check if there is a default dataset from environment variable
      # but don't use if there is a default server set that is different from the one
      # specified in the connection object
      defaultServer=unlist(getenvoroption("server"))
      if(is.null(defaultServer) ||
          isTRUE(all.equal(parse_url(conn$server), parse_url(defaultServer))))
        dataset = unlist(getenvoroption("dataset"))
      if (is.null(dataset) || nchar(dataset)<1) {
        if (length(datasets4conn) == 0)
          stop(
            "I'm sorry I can't find a default dataset for your current neuPrint connection.\n",
            "Please supply a `dataset` argument or set a default one using the ",
            "neuprint_dataset environment variable!\nSee ?neuprint_login for details."
          )
        dataset = datasets4conn[1]
        if (length(datasets4conn) > 1) {
          warning(call.=F,
            "Please supply a `dataset` argument or set a default one at login or using the ",
            "neuprint_dataset environment variable! See ?neuprint_login for details.",
            " For now we will use '",
            dataset,"'."
          )
        }
      }
    }
    if(length(datasets4conn) && !dataset %in% datasets4conn){
      stop("Specified dataset: `", dataset, "` does not match those provided by your ",
      "neuPrint connection:\n  ", paste(datasets4conn, collapse=", "),
      "\nSee ?neuprint_login for details.")
    }
    dataset
  }

# return available datasets sorted my descending modification time
# i.e. newest first
available_datasets <- function(conn=NULL, ...) {
  conn=neuprint_login(conn)
  ds=neuprint_datasets_memo(conn=conn, ...)
  if(length(ds)==0) return(NULL)
  # find last modification times, filling missing values with na
  lastmod = sapply(ds, function(x) {
    lm = x[["last-mod"]]
    if (nzchar(lm)) lm else NA_character_
  })
  # sort to return newest first
  lastmod=sort(lastmod, na.last = TRUE, decreasing = TRUE)
  datasets=names(lastmod)
  datasets
}
