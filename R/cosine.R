check_coconat <- function() {
  if(!requireNamespace("coconat", quietly = TRUE))
    stop("Please install suggested natverse coconat package! For example do:\n",
         "natmanager::install(pkgs = 'coconat')")
}

#' Calculate a cosine similarity matrix for neuprint neurons
#'
#' @param ids Passed to \code{\link{neuprint_ids}}
#' @param ... Optional filter expression defining which partners to include
#' @param threshold An integer threshold (connections >= this will be returned)
#' @param partners Whether to cluster based on connections to input or output
#'   partner neurons (default both).
#' @inheritParams neuprint_fetch_custom
#'
#' @return matrix or list of two matrices (input and output)
#' @export
#'
#' @examples
#' \donttest{
#' neuprint_cosine_matrix("/lLN2.+", grepl("PN",type), partners='in')
#' }
neuprint_cosine_matrix <- function(ids, ..., threshold=5,
                                   partners = c("outputs", "inputs"),
                                   conn=NULL) {
  check_coconat()
  partners=match.arg(partners, several.ok = T)
  conn=neuprintr::neuprint_login(conn = conn)
  ids=neuprintr::neuprint_ids(ids, unique = T, must_work = T, conn=conn)
  # i.e. no filtering
  if(!(length(ids)>0))
    stop("No valid ids provided!")

  cell_types=!missing(...)

  if('inputs' %in% partners) {
    fpsin=
      neuprintr::neuprint_connection_table(ids, partners = "in", summarise = T, details = cell_types, conn=conn, threshold = threshold) %>%
      filter(...) %>%
      dplyr::mutate(direction='in')
    fami <- coconat::partner_summary2adjacency_matrix(fpsin, )
    famicos=coconat::cosine_sim(fami, transpose = F)
  }
  if("outputs" %in% partners) {
    fpsout=neuprintr::neuprint_connection_table(ids, partners = "out", summarise = T, details = cell_types, threshold = threshold, conn=conn) %>%
      dplyr::filter(...) %>%
      dplyr::mutate(direction='out')
    famo <-
      coconat::partner_summary2adjacency_matrix(fpsout, inputids = ids)
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
#' @param x Query ids. May including searches in the style of
#'   \code{\link{neuprint_ids}}
#' @param nas What to do with entries that have NAs. Default is to set them to 0
#'   similarity.
#' @param method The cluster method to use (see \code{\link{hclust}})
#' @param labRow A vector defining the row labels. If it is a character vector
#'   length 1 containing any curly braces it will be interpreted as a
#'   \code{\link{glue}} string for interpolation, using the data.frame returned
#'   by \code{\link{neuprint_get_meta}} as a source of information.
#' @param ... Additional arguments passed to \code{\link{heatmap}}
#' @inheritParams neuprint_cosine_matrix
#' @inheritParams stats::heatmap
#'
#' @return The result of \code{\link{heatmap}} invisibly including the row and
#'   column dendrograms.
#' @export
#'
#' @examples
#' \donttest{
#' # Considering all partners
#' neuprint_cosine_matrix("/lLN2.+", partners='in') %>% neuprint_cosine_plot()
#' # excluding the PN partners from the cosine distance score
#' neuprint_cosine_matrix("/lLN2.+", !grepl("PN",type), partners='in') %>% neuprint_cosine_plot()
#' # just use PN partners for cosine distance score
#' neuprint_cosine_matrix("/lLN2.+", grepl("PN",type), partners='in') %>% neuprint_cosine_plot()
#' }
neuprint_cosine_plot <- function(x, partners=c('inputs', 'outputs'), threshold=5,
                                 method=c("ward.D", "single", "complete", "average",
                                          "mcquitty", "median", "centroid", "ward.D2"),
                                 labRow='{type}',
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
                             threshold=threshold)
  }
  x <- coconat::prepare_cosine_matrix(x, partners = partners, action=nas)

  if(is.character(labRow) && length(labRow)==1 && any(grepl("\\{", labRow))) {
    ci=neuprintr::neuprint_get_meta(rownames(x), conn=conn)
    labRow <- glue::glue(labRow, .envir = ci)
  }

  stats::heatmap(x,
          distfun = function(x) as.dist(1-x),
          hclustfun = function(...) hclust(..., method=method),
          symm = T, keep.dendro = T,
          labRow=labRow,
          ...)
}