#' @section Login: See \code{\link{neuprint_login}} or the package README for
#'   details of the how to login to a neuPrint server, the recommended initial
#'   setup and (optionally) how to specify a default dataset.
#'
#' @section Online Documentation: See
#'   \url{https://natverse.github.io/neuprintr/} for structured online
#'   documentation.
#'
#' @section Package Options: \itemize{
#'
#'   \item \code{neuprintr.support_hidden_rois} In some instances rois are not
#'   defined in a dataset's ROI list but only in field names. However, in these
#'    circumstances, these ROI fields will not be listed in the neuprint field
#'    properties for the dataset and a slower approach must be used to list
#'    fields. To avoid paying a speed penalty for what is a rare circumstance
#'    even with private in progress datasets, this option is provided. Set it to
#'    \code{TRUE} to support such hidden fields if you need them.
#'   }
#'
#' @examples
#' # List all package options
#' \dontrun{
#' options()[grepl("^neuprintr", names(options()))]
#' }
#'
#' @keywords internal
#' @aliases neuprintr
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
## usethis namespace: end
NULL
