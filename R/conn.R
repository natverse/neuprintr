#' @title Connect to/authenticate with a neuPrint server and its Neo4j database,
#'   returning a neuPrint connection object
#'
#' @description \code{neuprint_login} allows you to login to a neuPrint server
#'   specified by a \code{neuprint_connection} object. If such an object is not
#'   specified, then the last successful connection in this R session is reused
#'   if possible otherwise a new connection object is created using
#'   \code{environment variables} of the form "neuprint_*" (see details).
#'
#'   If your server has more than one dataset available, it is also a good idea
#'   to set the default neuPrint dataset you want to work with, either by
#'   passing an explicit \code{dataset} argument (to \code{neuprint_login} or
#'   \code{neuprint_connection}) when first making the connection or by setting
#'   a \code{neuprint_dataset} environment variable.
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
#'   server's address \code{/token}, once you have signed in via your approved
#'   Google account. Contact the server's administrators if you do not have
#'   access, but think that you should. You can then set the
#'   \code{catmaid.token} package option, but no
#'
#'   Note that you must \bold{NOT} reveal this token e.g. by checking it into a
#'   version controlled script as it gives complete access to your neuPrint
#'   account.
#' @param server the neuprint server
#' @param token your personal Bearer token
#' @param dataset A default dataset to use with this connection (you can still
#'   override this using the \code{dataset} argument of other \code{neuprintr}
#'   functions.)
#' @param conn a neuprintr connection object
#' @param config an \code{httr::\link[httr]{config}} object that can be used to
#'   set advanced curl options (e.g. additional authentication, proxy settings
#'   etc). See \bold{Curl options} section and \bold{Examples}.
#' @param Cache Whether to cache open connections at login so that they can be
#'   reused automatically.
#' @param Force Whether to force a new login to the CATMAID server (default
#'   \code{FALSE})
#' @param ... methods passed to \code{neuprint_connection}
#' @return a \code{neuprint_connection} object that can be used to make
#'   authenticated https requests to a neuPrint server, specifically by making
#'   use of its \code{$config} field.
#'
#' @section Environment variables:
#'
#'   You will very likely want to set the following environment variables in
#'   your \code{.Renviron} file (see \code{\link{Startup}} for details). This
#'   file is read by R on startup. In this way the neuprintr package will
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
#'   \preformatted{neuprint_server = "https://neuprint.janelia.org"
#'   neuprint_token =
#'   "asBatEsiOIJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImFsZXhhbmRlci5zaGFrZWVsLmJhdGVzQGdtYWlsLmNvbSIsImxldmVsIjoicmVhZHdyaXRlIiwiaW1hZ2UtdXJsIjoiaHR0cHM7Ly9saDQuZ29vZ2xldXNlcmNvbnRlbnQuY29tLy1QeFVrTFZtbHdmcy9BQUFBQUFBQUFBDD9BQUFBQUFBQUFBQS9BQ0hpM3JleFZMeEI4Nl9FT1asb0dyMnV0QjJBcFJSZlBRL21vL3Bob3RvLapwZz9zej01MCIsImV4cCI6MTczMjc1MjU2HH0.jhh1nMDBPl5A1HYKcszXM518NZeAhZG9jKy3hzVOWEU"}
#'
#'   and \bold{must} finish with a return at the end of the last line. Your
#'   \code{neuprint_token} is unique to you and must be obtained from a neuPrint
#'   web page once you have logged in with an approved Google account.
#'
#'   The use of the \code{neuprint_dataset} environment variable is optional and
#'   only recommended when your default neuprint server has more than one
#'   dataset. This default will \emph{not} apply to connections that refer to a
#'   server other than the one specified by the \code{neuprint_server}
#'   environment variable.
#'
#'   \preformatted{neuprint_dataset = "hemibrain:v1.0"}
#'
#' @section Options: Although setting environment variables is the recommended
#'   approach, you can also set R startup options e.g. in your \code{.Rprofile}
#'   to specify default neuPrint login options including your personal access
#'   token. The startup options have the same names as the environment variables
#'   listed above, so you can place code along the lines of:
#'
#'   \code{options(neuprint_server = 'https://neuprint.janelia.org',
#'   neuprint_token =
#'   "asBatEsiOIJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImFsZXhhbmRlci5zaGFrZWVsLmJhdGVzQGdtYWlsLmNvbSIsImxldmVsIjoicmVhZHdyaXRlIiwiaW1hZ2UtdXJsIjoiaHR0cHM7Ly9saDQuZ29vZ2xldXNlcmNvbnRlbnQuY29tLy1QeFVrTFZtbHdmcy9BQUFBQUFBQUFBDD9BQUFBQUFBQUFBQS9BQ0hpM3JleFZMeEI4Nl9FT1asb0dyMnV0QjJBcFJSZlBRL21vL3Bob3RvLapwZz9zej01MCIsImV4cCI6MTczMjc1MjU2HH0.jhh1nMDBPl5A1HYKcszXM518NZeAhZG9jKy3hzVOWEU")}
#'
#'   in your \code{.Rprofile} (see \code{\link{Startup}} for details). Note that
#'   it is important to have a final return at the end of your \code{.Rprofile}
#'   file.
#' @section Curl options: \bold{neuprintr} uses the curl library provided by the
#'   \code{httr} and \code{curl} packages to carry out remote requests. You can
#'   set curl options by passing an \code{httr::\link[httr]{config}} object that
#'   can be used to set advanced curl options (e.g. additional authentication,
#'   proxy settings etc). See \code{\link[curl]{handle}} and
#'   \code{\link[curl]{curl_options}} for a full list of possible options.
#'
#'   You can also set default curl options using environment variables with
#'   names of the form \code{neuprint_curl_<curloption>}. For example the
#'   following entry in you \code{\link{Renviron}} file will set the curl
#'   \code{ssl_verifyhost} option:
#'
#'   \verb{neuprint_curl_ssl_verifyhost=0}

#' @seealso \code{\link{options}}, \code{\link{Startup}},
#'   \code{\link{neuprint_datasets}}
#' @examples
#' \dontrun{
#' ## example explicitly specifying connection options
#' conn = neuprint_login(server= "neuprint.janelia.org",
#'   token= "asBatEsiOIJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImFsZXhhbmRlci5za
#'   GFrZWVsLmJhdGVzQGdtYWlsLmNvbSIsImxldmVsIjoicmVhZHdyaXRlIiwiaW1hZ2UtdXJsIj
#'   oiaHR0cHM7Ly9saDQuZ35vZ2xldXNlcmNvbnRlbnQuY29tLy1QeFVrTFZtbHdmcy9BQUFBQUF
#'   BQUFBDD9BQUFBQUFBQUFBQS9BQ0hpM3JleFZMeEI4Nl9FT1asb0dyMnV0QjJBcFJSZlBRL21v
#'   L3Bob3RvLapwZz9zej01MCIsImV4cCI6MTczMjc1MjM2HH0.jhh1nMDBPl5A1HYKcszXM518NZ
#'   eAhZG9jKy3hzVOWEU")
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
#'
#' # make connection to second server
#' conn2=neuprint_login(server="https://server2.org",
#'   token=Sys.getenv('NPSERVER2'))
#'
#' # specify a default dataset (only required when >1 dataset available)
#' conn2=neuprint_login(server="https://server2.org",
#'   token=Sys.getenv('NPSERVER2'), dataset="hemibrain")
#' }
#' @export
#' @rdname neuprint_login
neuprint_connection <- function(server=NULL, token=NULL, dataset=NULL,
                                conn=NULL, config=httr::config()) {
  if (!is.null(conn))
    return(conn)

  # Set a default server if none specified
  neuprint_server <-
    if(is.null(server)) unlist(getenvoroption("server")) else server
  # we will always add one in our calls
  neuprint_server <- remove_trailing_slash(neuprint_server)
  # Set a default token if none specified
  neuprint_token <- if(is.null(token)) unlist(getenvoroption("token")) else token
  # collect any curl options defined as environment variables
  config=neuprint_curl_options(config)
  conn=list(server = neuprint_server, token = neuprint_token, config=config,
            dataset=dataset)
  class(conn)='neuprint_connection'
  conn
}

#' @export
print.neuprint_connection <- function(x, ...) {
  cat("Connection to neuPrint server:\n  ",
      x$server, sep="", "\n")
  if(!is.null(x$dataset))
    cat("with default dataset:\n  ", x$dataset, "\n")
  if(!is.null(x$authresponse)) {
    cat("Login active since:", httr::headers(x$authresponse)$date)
  } else {
    cat("No active login")
  }
  invisible(x)
}

remove_trailing_slash <- function(x) sub("/$", "", x)

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
#' @name neuprint_login
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
  } else {
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
    canonurl=remove_trailing_slash(conn$authresponse$url)
    if(!isTRUE(conn$server==canonurl)) {
      warning("The URL reported by neuprint server differs from what you specified:\n\n",
              paste("  The server URL that you provided : ", conn$server, "\n"),
              paste("  Canonical URL according to server: ", canonurl, "\n\n"),
              "I will update the URL in this neuprint connection to the canonical URL\n",
              "and recommend that you change your configuration to match.\n",
              "See ?neuprint_login for more details."
              )
      conn$server=canonurl
    }
  }
  conn$cookies = unlist(httr::cookies(conn$authresponse))
  conn$config = c(conn$config, httr::set_cookies(conn$cookies))
  # set a default dataset if none exists / check one specified in connection
  conn$dataset=check_dataset(conn = conn)
  if (Cache)
    neuprint_cache_connection(conn)
  invisible(conn)
}

# Hidden
getenvoroption <- function(vars, prefix="neuprint_", ignore.case=TRUE){
  fullvars=paste0(prefix, vars)
  res <- if(isTRUE(ignore.case)) {
    fullvars=tolower(fullvars)
    envs=Sys.getenv(names=T)
    envsc=envs
    names(envsc)=tolower(names(envs))
    c(envsc[fullvars])
  } else {
    Sys.getenv(fullvars, names = T, unset = NA)
  }

  if(all(is.na(res))){
    # no env variables are set, let's try options
    res=do.call(options, as.list(fullvars))
  } else {
    # convert environment variables into options style list
    res=as.list(res)
    # replace missing values with NULL
    res=sapply(res, function(x) if(is.na(x)) NULL else x, simplify = F)
  }
  # give result the original variable names
  names(res)=vars
  res
}

# for curl options defined as environment variables
neuprint_curl_options <- function(extra_opts=httr::config()) {
  envs=Sys.getenv()
  curlopts=envs[grepl("^neuprint_curl_", names(envs))]
  if (length(curlopts)) {
    names(curlopts) = sub("neuprint_curl_", "", names(curlopts))
  } else {
    curlopts = list()
  }
  keep=setdiff(names(curlopts), names(extra_opts$options))
  curlopts=as.list(curlopts[keep])
  # environment variables come in as strings, but sometimes we want numbers
  curlopts <- sapply(curlopts, function(x) switch(x, `0`=0L, `1`=1L, x), simplify = F)
  c(extra_opts, do.call(httr::config, curlopts))
}

