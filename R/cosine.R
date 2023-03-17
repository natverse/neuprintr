check_coconat <- function() {
  if(!requireNamespace("coconat", quietly = TRUE))
    stop("Please install suggested natverse coconat package! For example do:\n",
         "natmanager::install(pkgs = 'coconat')")
}

#' Calculate a cosine similarity matrix for neuprint neurons
#'
#' @details For most purposes you can use \code{\link{neuprint_cosine_plot}}
#'   directly, but it can sometimes be useful to use
#'   \code{neuprint_cosine_matrix} to have more control over how partner neurons
#'   are grouped (see e.g. \code{groupfun}) or which partner neurons are
#'   included in the results (.
#'
#'   The \code{groupfun} argument can be a powerful way to construct flexible
#'   grouping strategies for partner neurons. It was added in order to use
#'   information present in fields such as the group, serial or instance/name
#'   columns in the male VNC/CNS datasets. It will receive as input a dataframe
#'   and expects to receive a single vector of length matching the number of
#'   rows in the input dataframe. The input dataframe will contain the standard
#'   columns returned by \code{\link{neuprint_connection_table}} but you can
#'   request extra columns if necessary by naming them in the \code{group}
#'   argument.
#' @param ids Passed to \code{\link{neuprint_ids}}
#' @param ... Optional filter expression defining which partners to include
#' @param threshold An integer threshold (connections >= this will be returned)
#' @param group Whether to group by cell \code{type} or another named column.
#' @param groupfun A function which receives the metadata for all partner
#'   neurons and returns a single grouping vector (see the \bold{details}
#'   section).
#' @param details Optional character vector naming metadata columns to fetch for
#'   partner neurons.
#' @param partners Whether to cluster based on connections to input or output
#'   partner neurons (default both).
#' @inheritParams neuprint_fetch_custom
#'
#' @return matrix or list of two matrices (input and output)
#' @export
#' @seealso \code{\link{neuprint_cosine_plot}}
#' @examples
#' \donttest{
#' # NB the second (unnamed argument) filters the partner neurons
#' # so that only those with type containing the regular expression ORN are used
#' neuprint_cosine_matrix("/DA[1-3].*PN", grepl("ORN",type), partners='in')
#' }
neuprint_cosine_matrix <- function(ids, ..., threshold=5,
                                   partners = c("outputs", "inputs"),
                                   group=FALSE,
                                   groupfun=NULL,
                                   details=NULL,
                                   conn=NULL) {
  check_coconat()
  partners=match.arg(partners, several.ok = T)
  conn=neuprintr::neuprint_login(conn = conn)
  ids=neuprintr::neuprint_ids(ids, unique = T, must_work = T, conn=conn)
  # i.e. no filtering
  if(!(length(ids)>0))
    stop("No valid ids provided!")

  if(isTRUE(group))
    group='type'
  if(is.null(details)) {
    if(missing(...) && isFALSE(group) && is.null(groupfun))
      details=FALSE
    else {
      details=c("type", "instance")
      if(!isFALSE(group))
        details=union(details, group)
    }
  }

  if('inputs' %in% partners) {
    fpsin=
      neuprintr::neuprint_connection_table(ids, partners = "in", details = details, conn=conn, threshold = threshold) %>%
      filter(...) %>%
      dplyr::mutate(direction='in')
    fami <- coconat::partner_summary2adjacency_matrix(
      fpsin,
      inputcol = ifelse(!isFALSE(group), group, 'partner'),
      inputids = groupfun,
      outputcol = 'bodyid',
      outputids = ids,
      standardise_input = F
    )

    famicos=coconat::cosine_sim(fami, transpose = F)
  }
  if("outputs" %in% partners) {
    fpsout=neuprintr::neuprint_connection_table(ids, partners = "out", details = details, threshold = threshold, conn=conn) %>%
      dplyr::filter(...) %>%
      dplyr::mutate(direction='out')
    famo <-
      coconat::partner_summary2adjacency_matrix(
        fpsout,
        inputcol = 'bodyid',
        outputcol = ifelse(!isFALSE(group), group, 'partner'),
        outputids = groupfun,
        inputids = ids,
        standardise_input = F
      )
    famocos=coconat::cosine_sim(famo, transpose = T)
  }
  if(length(partners)==2)
    list(cin=famicos, cout=famocos, win=sum(fpsin$weight), wout=sum(fpsout$weight))
  else if(partners=='inputs')
    famicos
  else
    famocos
}

#' Plot cosine clustering of neuprint/hemibrain neurons
#'
#' @details Note that when \code{interactive=TRUE} you must have external
#'   packages including \code{InteractiveComplexHeatmap} installed with the
#'   \code{coconat} package. If you are using Rstudio, we recommend using an
#'   external browser (e.g. Chrome) rather than the built-in browser, especially
#'   for larger heatmaps.
#'
#' @param x Query ids. May including searches in the style of
#'   \code{\link{neuprint_ids}}
#' @param nas What to do with entries that have NAs. Default is to set them to 0
#'   similarity.
#' @param method The cluster method to use (see \code{\link{hclust}})
#' @param labRow A vector defining the row labels. If it is a character vector
#'   length 1 containing any curly braces it will be interpreted as a
#'   \code{\link{glue}} string for interpolation, using the data.frame returned
#'   by \code{\link{neuprint_get_meta}} as a source of information.
#' @param interactive Whether to plot an interactive heatmap (allowing zooming
#'   and id selection). See details.
#' @param ... Additional arguments passed to \code{\link{heatmap}}
#' @inheritParams neuprint_cosine_matrix
#' @inheritParams stats::heatmap
#'
#' @return The result of \code{\link{heatmap}} invisibly including the row and
#'   column dendrograms.
#' @export
#' @seealso \code{\link{neuprint_cosine_matrix}}
#' @examples
#' \donttest{
#' neuprint_cosine_plot("/DA[1-3].*PN", partners='in')
#' }
#' \dontrun{
#' # Considering all partners
#' neuprint_cosine_matrix("/lLN2.+", partners='in') %>% neuprint_cosine_plot()
#' # excluding the PN partners from the cosine distance score
#' neuprint_cosine_matrix("/lLN2.+", !grepl("PN",type), partners='in') %>% neuprint_cosine_plot()
#' # just use PN partners for cosine distance score
#' neuprint_cosine_matrix("/lLN2.+", grepl("PN",type), partners='in') %>% neuprint_cosine_plot()
#'
#' # interactive heatmap allowing zooming and id selection
#' neuprint_cosine_plot("/lLN2.+",  partners='out', interactive=TRUE)
#' }
neuprint_cosine_plot <- function(x, partners=c('inputs', 'outputs'), threshold=5,
                                 method=c("ward.D", "single", "complete", "average",
                                          "mcquitty", "median", "centroid", "ward.D2"),
                                 group=FALSE,
                                 labRow='{type}',
                                 interactive=FALSE,
                                 nas=c('zero','drop'),
                                 conn=NULL,
                                 ...) {
  check_coconat()
  method=match.arg(method)
  partners=match.arg(partners, several.ok = T)
  cx=class(x)
  ismatlist=is.list(x) && length(x)==4 && all(c("cin", "cout") %in% names(x))
  ismat=is.matrix(x) || isTRUE(attr(cx, "package") == "Matrix")

  if(!ismatlist && !ismat) {
    x=neuprint_cosine_matrix(x, conn = conn, partners = partners,
                             threshold=threshold, group=group)
  }
  x <- coconat::prepare_cosine_matrix(x, partners = partners, action=nas)

  if(is.character(labRow) && length(labRow)==1 && any(grepl("\\{", labRow))) {
    ci=neuprintr::neuprint_get_meta(rownames(x), conn=conn)
    labRow <- glue::glue(labRow, .envir = ci)
  }

  if(interactive) {
    try(cv <- requireNamespace('coconat', versionCheck=list(op='>', version='0.1.0')))
    if(inherits(cv, 'try-error'))
      stop("Please install/update suggested package coconat.\n",
           "natmanager::install(pkgs = 'coconat')\n","is a good way to do this")
  }
  coconat:::cosine_heatmap(x, interactive = interactive, labRow = labRow, method = method, ...)
}
