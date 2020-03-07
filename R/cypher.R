#' @title Use a cypher to make a custom query to the neuPrint server specifying
#'   the information you want to obtain
#'
#' @description  Get summary information about the datasets hosted by the
#'   neuPrint server in which you are interested
#' @param cypher the cypher by which to make your search, the default returns
#'   the available datasets and the servers that host their associated mesh data
#' @param cache if \code{TRUE} will use memoisation to cache the result of the
#'   call for 1 hour.
#' @param conn optional, a neuprintr connection object, which also specifies the
#'   neuPrint server. If NULL, the defaults set in your
#'   \code{\link[=Startup]{.Rprofile}} or \code{\link[=Startup]{.Renviron}} are
#'   used. See \code{\link{neuprint_login}} for details.
#' @param dataset optional, a dataset you want to query. If \code{NULL}, the
#'   default specified by your R environ file is used or, failing that the
#'   current connection, is used. See \code{\link{neuprint_login}} for details.
#' @param ... methods passed to \code{neuprint_login}
#' @seealso \code{\link{neuprint_login}}, \code{\link{neuprint_available}}
#' @export
#' @rdname neuprint_fetch_custom
neuprint_fetch_custom <- function(cypher = "MATCH (n:Meta) RETURN n.dataset, n.meshHost",
                                  dataset = NULL, conn = NULL, cache=FALSE, ...){
  conn=neuprint_login(conn)
  dataset = check_dataset(dataset, conn=conn)
  Payload <- sprintf('{"cypher":"%s","dataset":"%s"}', cypher, dataset)
  class(Payload) <- "json"
  if(cache) {
    neuprint_fetch_memo(path = 'api/custom/custom',
                        body = Payload,
                        conn = conn,
                        ...)
  } else {
    neuprint_fetch(path = 'api/custom/custom',
                   body = Payload,
                   conn = conn,
                   ...)
  }
}

