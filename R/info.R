#' @title Find out what some information about your neuPrint server
#'
#' @description  Get summary information about the datasets hosted by the neuPrint server in which you are interested
#' @param conn optional, a neuprintr connection object, which also specifies the neuPrint server see \code{?neuprint_login}.
#' If NULL, your defaults set in your R.profile or R.environ are used.
#' @param ... methods passed to \code{neuprint_login}
#' @seealso \code{\link{neuprint_login}}
#' @export
#' @rdname neuprint_info
#' @examples
#' \donttest{
#' # list details for all the available datasets
#' neuprint_datasets()
#' }
neuprint_datasets <- function(conn = NULL, ...){
  neuprint_fetch(path = 'api/dbmeta/datasets', conn = conn, simplifyVector = TRUE, include_headers = FALSE, ...)
}

#' @export
#' @rdname neuprint_info
neuprint_database <- function(conn = NULL, ...){
  neuprint_fetch(path = 'api/dbmeta/database', conn = conn,
                 simplifyVector = TRUE, include_headers = FALSE, ...)
}

#' @export
#' @rdname neuprint_info
#' @examples
#' \donttest{
#' # list API endpoints
#' head(neuprint_available())
#' }
neuprint_available <- function(conn = NULL, ...){
  av = neuprint_fetch(path = 'api/available', conn = conn, ...)
  do.call(rbind,av)
}

#' @export
#' @rdname neuprint_info
#' @examples
#' \donttest{
#' neuprint_version()
#' }
neuprint_version <- function(conn = NULL, ...){
  neuprint_fetch(path = 'api/version', conn = conn, include_headers=FALSE,  ...)
}

#' @title Get a vector of all the ROIs in a specified dataset
#'
#' @description Get the regions of interest (ROIs) used in a neuPrint project
#' @param superlevel whether not to show 'superlevel' ROIs - ROIs composed of other ROIs.
#' @param dataset optional, a dataset you want to query. If NULL, the default
#'   specified by your R environ file is used. See \code{neuprint_login} for
#'   details.
#' @param conn optional, a neuprintr connection object, which also specifies the
#'   neuPrint server see \code{\link{neuprint_login}}. If NULL, your defaults
#'   set in your R.profile or R.environ are used.
#' @param ... methods passed to \code{neuprint_login}
#' @seealso \code{\link{neuprint_login}}, \code{\link{neuprint_datasets}}
#' @export
neuprint_ROIs <- function(superLevel = FALSE, dataset = NULL, conn = NULL, ...){
  ds = neuprint_datasets(conn=conn, ...)
  dataset = check_dataset(dataset)
  if(superLevel){
    rois = ds[[dataset]]$superLevelROIs
  }else{
    rois = ds[[dataset]]$ROIs
  }
  sort(rois)
}



