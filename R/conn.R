#' @title Connect to/authenticate with a neuPrint server and its Neo4j database,
#'   returning a neuPrint connection object
#'
#' @description \code{neuprint_login} allows you to login to a neuPrint server
#'   specified by a \code{neuprint_connection} object. If such an object is not
#'   specified, then the last successful connection in this R session is reused
#'   if possible otherwise a new connection object is created using
#'   \code{options} of the form "neuprint_*" (see details). It is also very
#'   useful to set the default neuPrint dataset you want to work with, if the
#'   server hosts multiple datasets, see details.
#'
#'   The connection object returned by \code{neuprint_login} (or cached when
#'   \code{Cache=TRUE}, the default) can then be used for future requests to the
#'   neuPrint server by get/query/fetch functions.
#'
#' @details After successful login, the \code{neuprint_connection} object will
#'   contain a \code{cookie} field that includes a sessionid that is required
#'   for subsequent GET/POST operations using the package \code{httr}. When
#'   \code{Cache=TRUE} (the default) the open connection object is cached and
#'   will be used when EITHER \code{neuprint_login} is called with enough
#'   information to indicate that the same server is desired OR (when no
#'   information about the server is passed to \code{neuprint_login}) the last
#'   opened connection will be used. A new connection can be made using
#'   \code{Force = TRUE}, which is advisable as a first call for debugging if
#'   you are having issues querying the server.
#'
#' @section Token based authentication: neuPrint requires Bearer token based
#'   authentication. You can get your token by going to your neuPrint server's
#'   webpage and right clicking on the icon showing your Google account on the
#'   top right corner, and selecting \bold{AUTH_TOKEN}, or often at your
#'   server's address \code{/token}, once you havw signed in via your approved
#'   Google account. Contact the server's administrators if you do not have
#'   access, but think that you should. You can then set the
#'   \code{catmaid.token} package option, but no
#'
#'   Note that you must \bold{NOT} reveal this token e.g. by checking it into a
#'   version controlled script as it gives complete access to your neuPrint
#'   account.
#' @param server the neuprint server
#' @param token your personal Bearer token
#' @param conn a neuprintr connection object
#' @param config an \code{httr::\link[httr]{config}} object that can be used to
#'   set advanced curl options (e.g. additional authentication, proxy settings
#'   etc)
#' @param Cache Whether to cache open connections at login so that they can be
#'   reused automatically.
#' @param Force Whether to force a new login to the CATMAID server (default
#'   \code{FALSE})
#' @param ... methods passed to neuprint_connection
#' @return a \code{neuprint_connection} object that can be used to make
#'   authenticated https requests to a neuPrint server, specifically by making
#'   use of its \code{$config} field.
#'
#' @section Environment variables:
#'
#'   You will very likely want to set the following environment variables in
#'   your \code{.Renviron} file (see \code{\link{Startup}} for details). This
#'   file is read by R on startup. In this way the catmaid package will
#'   automatically login to your preferred neuPrint server. Note that
#'   environment variables will also be inherited by child R sessions. This
#'   means for example that they will be available when running knitr reports,
#'   tests or R CMD Check from RStudio. In order to edit your R.profile or
#'   R.environ files easily and directly, try using
#'   \code{usethis::edit_r_environ()} and \code{usethis::edit_r_profile()}
#'
#'   \itemize{
#'
#'   \item{\code{neuprint_server}}
#'
#'   \item{\code{neuprint_token}}
#'
#'   \item{\code{neuprint_dataset}}
#'
#'   } An example \code{.Renviron} file might look like:
#'
#'   \preformatted{neuprint_server = "https://emdata1.int.janelia.org:11000"
#'   neuprint_token =
#'   "asBatEsiOIJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImFsZXhhbmRlci5zaGFrZWVsLmJhdGVzQGdtYWlsLmNvbSIsImxldmVsIjoicmVhZHdyaXRlIiwiaW1hZ2UtdXJsIjoiaHR0cHM7Ly9saDQuZ29vZ2xldXNlcmNvbnRlbnQuY29tLy1QeFVrTFZtbHdmcy9BQUFBQUFBQUFBDD9BQUFBQUFBQUFBQS9BQ0hpM3JleFZMeEI4Nl9FT1asb0dyMnV0QjJBcFJSZlBRL21vL3Bob3RvLapwZz9zej01MCIsImV4cCI6MTczMjc1MjU2HH0.jhh1nMDBPl5A1HYKcszXM518NZeAhZG9jKy3hzVOWEU"
#'    neuprint_dataset = "hemibrain"}
#'
#'   and \bold{must} finish with a return at the end of the last line. Your
#'   \code{neuprint_token} is unique to you and must be obtained from a neuPrint
#'   web page once you havw logged in with an approved Google account.
#'
#' @section Options: Although setting environment variables is the recommended
#'   approach, you can also set R startup options e.g. in your \code{.Rprofile}
#'   to specify default neuPrint login options including your personal access
#'   token. The startup options have the same names as the environment variables
#'   listed above, so you can place code along the lines of:
#'
#'   \code{options(neuprint_server = 'https://emdata1.int.janelia.org:11000',
#'   neuprint_token =
#'   "asBatEsiOIJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImFsZXhhbmRlci5zaGFrZWVsLmJhdGVzQGdtYWlsLmNvbSIsImxldmVsIjoicmVhZHdyaXRlIiwiaW1hZ2UtdXJsIjoiaHR0cHM7Ly9saDQuZ29vZ2xldXNlcmNvbnRlbnQuY29tLy1QeFVrTFZtbHdmcy9BQUFBQUFBQUFBDD9BQUFBQUFBQUFBQS9BQ0hpM3JleFZMeEI4Nl9FT1asb0dyMnV0QjJBcFJSZlBRL21vL3Bob3RvLapwZz9zej01MCIsImV4cCI6MTczMjc1MjU2HH0.jhh1nMDBPl5A1HYKcszXM518NZeAhZG9jKy3hzVOWEU",
#'    neuprint_dataset = 'hemibrain' )}
#'
#'   in your \code{.Rprofile} (see \code{\link{Startup}} for details). Note that
#'   it is important to have a final return at the end of your \code{.Rprofile}
#'   file.
#'
#' @seealso \code{\link{options}}, \code{\link{Startup}},
#'   \code{\link{neuprint_datasets}}
#' @examples
#' \dontrun{
#' ## example explicitly specifying connection options
#' # using modern token based authentication
#' conn = neuprint_login(server= "https://emdata1.int.janelia.org:11000",
#'   token= "asBatEsiOIJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImFsZXhhbmRlci5zaGFrZWVsLmJhdGVzQGdtYWlsLmNvbSIsImxldmVsIjoicmVhZHdyaXRlIiwiaW1hZ2UtdXJsIjoiaHR0cHM7Ly9saDQuZ29vZ2xldXNlcmNvbnRlbnQuY29tLy1QeFVrTFZtbHdmcy9BQUFBQUFBQUFBDD9BQUFBQUFBQUFBQS9BQ0hpM3JleFZMeEI4Nl9FT1asb0dyMnV0QjJBcFJSZlBRL21vL3Bob3RvLapwZz9zej01MCIsImV4cCI6MTczMjc1MjU2HH0.jhh1nMDBPl5A1HYKcszXM518NZeAhZG9jKy3hzVOWEU")
#'
#'
#' ## examples assuming that neuprint_* environment variables/options are set
#' conn = neuprint_login()
#'
#' ## using env vars + config to set advanced curl options
#' neuprint_login(config=httr::config(ssl_verifyhost=0))
#'
#' ## now do stuff with the connection like
#' available.datasets = neuprint_datasets(conn=conn)
#'
#' ## which, if you have edited your R.profile / R.environ, should produce the same results as
#' available.datasets = neuprint_datasets(conn=NULL)
#' }
#' @export
#' @rdname neuprint_login
neuprint_connection <- function(server=NULL, token=NULL, conn=NULL, config=httr::config()) {
  if (!is.null(conn))
    return(conn)
  # Set a default server if none specified
  defaultServer=unlist(getenvoroption("server"))
  if(missing(server)) {
    neuprint_server=defaultServer
  }
  # Set a default server if none specified
  defaultToken=unlist(getenvoroption("token"))
  if(missing(server)) {
    neuprint_token=defaultToken
  }
  conn=list(server = neuprint_server, token = neuprint_token, config=config)
  class(conn)='dv_conn'
  conn
}

# Hidden
neuprint_last_connection <- function(){
  conns = .neuprintr_statevars$connections
  num_conns = length(conns)
  if (num_conns)
    conns[[num_conns]]
  else NULL
}

# Hidden
neuprint_cached_connection <- function(conn=NULL){
  if (is.null(conn))
    return(NULL)
  open_connections = names(.neuprintr_statevars$connections)
  if (!length(open_connections))
    return(NULL)
  for (thisconn in open_connections) {
    thisconn = .neuprintr_statevars$connections[[thisconn]]
    checkfields = c("server", "username", "authname", "authtype")
    checkfields = checkfields[!sapply(conn[checkfields],
                                      is.null)]
    if (isTRUE(all.equal(thisconn[checkfields], conn[checkfields])))
      return(thisconn)
  }
  return(NULL)
}

# Hidden
neuprint_cache_connection <- function(conn){
  .neuprintr_statevars$connections[[neuprint_connection_fingerprint(conn)]] = conn
}

# Hidden
neuprint_connection_fingerprint <- function(conn){
  paste(c(conn$server, httr::cookies(conn$authresponse)),
        collapse = "")
}

#' @export
#' @rdname neuprint_login
neuprint_login <- function(conn = NULL, Cache = TRUE, Force = FALSE, ...){
  if (is.character(conn) && grepl("^http", conn)) {
    stop("To connect to : ", conn, ", you must name the server argument i.e.\n",
         sprintf("  neuprint_login(server=\"%s\")", conn))
  }
  if (is.null(conn)) {
    if (!length(pairlist(...))) {
      conn = neuprint_last_connection()
    }
    if (is.null(conn))
      conn = neuprint_connection(...)
  }
  if (!Force) {
    if (!is.null(conn$authresponse))
      return(invisible(conn))
    cached_conn = neuprint_cached_connection(conn)
    if (!is.null(cached_conn))
      return(invisible(cached_conn))
  }
  if (isTRUE(conn$nologin)) {
    conn$authresponse = httr::GET(url = conn$server)
    httr::stop_for_status(conn$authresponse)
    res_cookies = httr::cookies(conn$authresponse)
    GAPS_row = grepl("GAPS", res_cookies$name)
    if (any(GAPS_row)) {
      token_value = res_cookies$value[GAPS_row][1]
      conn$config = httr::add_headers(Authorization = token_value,
                                      referer = conn$server)
    }
    else warning("I can't seem to find a GAPS token.", "You will not be able to POST to this site!")
  }else {
    if(is.null(conn$config)) conn$config=httr::config()
    conn$config = c(
      conn$config,
      httr::add_headers(
        Authorization = paste0("Bearer ", conn$token),
        referer = conn$server,
        `Content-Type` = "application/json"
      )
    )
    conn$authresponse = httr::GET(url = conn$server,con=conn$config)
    httr::stop_for_status(conn$authresponse)
  }
  conn$cookies = unlist(httr::cookies(conn$authresponse))
  conn$config = c(conn$config, httr::set_cookies(conn$cookies))
  if (Cache)
    neuprint_cache_connection(conn)
  invisible(conn)
}

# Hidden
getenvoroption <- function(vars, prefix="neuprint_"){
  fullvars=paste0(prefix, vars)
  res=Sys.getenv(fullvars, names = T, unset = NA)
  if(all(is.na(res))){
    # no env variables are set, let's try options
    res=do.call(options, as.list(fullvars))
  } else {
    # convert environment variables into options style list
    res=as.list(res)
    # replace missing values with NULL
    res=sapply(res, function(x) if(is.na(x)) NULL else x)
  }
  # give result the original variable names
  names(res)=vars
  res
}
