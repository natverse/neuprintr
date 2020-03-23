#' @title Find bodies within a given ROI
#'
#' @description  Find bodies that input/output within certain ROIs (i.e. they must have synapses in these areas)
#' @param input_ROIs a vector of input ROIs. Use \code{neuprint_ROIs} to see what is available.
#' @param output_ROIs a vector of input ROIs. Use \code{neuprint_ROIs} to see what is available.
#' @param roi a single ROI. Use \code{neuprint_ROIs} to see what is available.
#' @param all_segments if TRUE, all bodies are considered, if FALSE, only 'Neurons', i.e. bodies with a status roughly traced status.
#' @param roi_check default TRUE. If FALSE, and roi isn't null, ignore ROI checking step
#' @param ... methods passed to \code{neuprint_login}
#' @inheritParams neuprint_fetch_custom
#' @return a n x n matrix, where the rows are input neurons and the columns are their targets
#' @examples
#' \donttest{
#' # Find some neurons with inputs in the Antennal Lobe (AL) and
#' # outputs in the LH
#' pncands=neuprint_find_neurons(input_ROIs = "AL(R)", output_ROIs = "LH(R)")
#' head(pncands)
#' plot(`LH(R).pre`~`AL(R).post`, data=pncands)
#' }
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_simple_connectivity}}, \code{\link{neuprint_common_connectivity}}
#' @export
#' @rdname neuprint_find_neurons
neuprint_find_neurons <- function(input_ROIs,
                                  output_ROIs = NULL,
                                  dataset = NULL,
                                  conn = NULL,
                                  all_segments = FALSE,
                                  roi_check=TRUE,
                                  ...){
  all_segments = ifelse(all_segments,"true","false")
  conn=neuprint_login(conn = conn)
  dataset = check_dataset(dataset, conn=conn)
  if (roi_check){
  roicheck = neuprint_check_roi(rois=unique(c(input_ROIs,output_ROIs)), dataset = dataset, conn = conn, ...)}
  Payload = noquote(sprintf('{"dataset":"%s","input_ROIs":%s,"output_ROIs":%s,"enable_contains":true,"all_segments":%s}',
                            dataset, jsonlite::toJSON(input_ROIs),
                            jsonlite::toJSON(output_ROIs),
                            all_segments))
  class(Payload) = "json"
  found.neurons = neuprint_fetch(path = 'api/npexplorer/findneurons', body = Payload, conn = conn, ...)
  columns = unlist(found.neurons[[1]])
  keep = !columns%in%c("roiInfo","rois")
  neurons = data.frame()
  extract = lapply(found.neurons[[2]], function(f) nullToNA(t(as.matrix(f[keep]))))
  neurons = as.data.frame(do.call(rbind, extract))
  colnames(neurons) = columns[keep]
  rownames(neurons) = neurons$bodyid
  roiInfoFields <- neuprint_get_fields(c("pre","post","downstream","upstream"),conn = conn,dataset=dataset,...)
  innervation = lapply(found.neurons[[2]], function(f)
    extract_connectivity_df(rois = c(input_ROIs,output_ROIs),
                            json=unlist(f[columns=="roiInfo"]),
                            postFix=roiInfoFields))
  innervation = do.call(rbind,innervation)
  neurons = cbind(neurons,innervation)
  as.data.frame(t(apply(neurons,1,unlist)),stringsAsFactors=FALSE)
}

#' @export
#' @rdname neuprint_find_neurons
#' @examples
#' \donttest{
#' lhr=neuprint_bodies_in_ROI('LH(R)')
#' head(lhr)
#' }
neuprint_bodies_in_ROI <- function(roi, roi_check=TRUE,dataset = NULL, all_segments = FALSE, conn = NULL, ...){
  all_segments = ifelse(all_segments,"Segment","Neuron")
  if (roi_check) roicheck = neuprint_check_roi(rois=roi, dataset = dataset, conn = conn, ...)
  cypher = sprintf("MATCH (n :`%s`) WHERE n.`%s` WITH n AS n, apoc.convert.fromJsonMap(n.roiInfo) AS roiInfo RETURN n.bodyId AS bodyid, n.size AS voxels, n.pre AS pre, n.post as post, roiInfo.`%s`.pre AS roipre, roiInfo.`%s`.post AS roipost",
                all_segments,
                roi,
                roi,
                roi)
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, dataset = dataset, ...)
  df=neuprint_list2df(nc, return_empty_df = T)
  df$roipre[is.na(df$roipre)]=0
  df$roipost[is.na(df$roipost)]=0
  df
}

#' Get connectivity between ROIs (summary or data frame of connecting neurons)
#'
#' @details When requesting summary connectivity data between ROIs, we recommend
#'   setting \code{cached=FALSE}. We have noticed small differences in the
#'   connections weights, but computation times can get very long for more than
#'   a handful of ROIs.
#' @param rois regions of interest for a dataset
#' @param full return all neurons involved (TRUE, the default) or give a numeric
#'   ROI summary (FALSE)
#' @param statistic either "weight" or count" (default "weight"). Which number
#'   to return (see neuprint explorer for details) for summary results (either
#'   (when \code{full=FALSE})
#' @param cached pull precomputed results (TRUE) or ask server to recalculate
#'   the connectivity (FALSE). Only applicable to summary results when
#'   \code{full=FALSE}.
#' @param roi_check default TRUE. If FALSE, and roi isn't null, ignore ROI checking step
#' @param ... methods passed to \code{neuprint_login}
#' @inheritParams neuprint_fetch_custom
#' @seealso \code{\link{neuprint_simple_connectivity}},
#'   \code{\link{neuprint_common_connectivity}}
#' @export
#' @examples
#' \donttest{
#' aba <- neuprint_ROI_connectivity(neuprint_ROIs(superLevel = TRUE),
#'   full=FALSE)
#' heatmap(aba)
#' }
neuprint_ROI_connectivity <- function(rois, full=TRUE,
                                      statistic = c("weight","count"),
                                      cached = !full,
                                      roi_check=TRUE,
                                      dataset = NULL, conn = NULL, ...) {
  statistic <- match.arg(statistic)
  if(isTRUE(full) && isTRUE(cached))
    stop("It is not possible to return a full list of connecting neurons when ",
         "`cached=TRUE`!\nPlease leave `cached` with its default value (FALSE).")
  if (roi_check) roicheck <- neuprint_check_roi(rois=rois, dataset = dataset, conn = conn, ...)
  if (cached) {
    results <-matrix(ifelse(statistic == 'count', 0L, 0),
                     nrow=length(rois), ncol=length(rois),
                     dimnames = list(inputs=rois,outputs=rois))
    roi.conn = neuprint_fetch(path = 'api/cached/roiconnectivity', conn = conn, ...)
    missing=setdiff(rois, unlist(roi.conn$roi_names))
    if(length(missing))
      warning("Dropping missing rois:", paste(missing, collapse = " "))
    allpairs = names(roi.conn$weights)
    for (inp in rois) {
      for (out in rois) {
        edgename=paste(inp, out, sep="=>")
        if(edgename %in% allpairs)
          results[inp,out] <- roi.conn$weights[[edgename]][[statistic]]
      }
    }
  } else {
    Payload = noquote(sprintf('{"dataset":"%s","rois":%s}',
                            dataset,
                            ifelse(is.null(rois),jsonlite::toJSON(list()),jsonlite::toJSON(rois))))
    class(Payload) = "json"
    roi.conn <- neuprint_fetch(path = 'api/npexplorer/roiconnectivity', body = Payload, conn = conn, ...)
    ll <- neuprint_list2df(roi.conn)
    # running fromJSON on many separate strings is slow, so start by
    # selecting strings that actually contain the selected ROIs
    hasroi=sapply(rois, function(roi)
      stringr::str_detect(ll$roiInfo, stringr::fixed(paste0('"',roi,'"'))))
    if(is.matrix(hasroi)) hasroi=rowSums(hasroi)>0

    roiInfoFields <- neuprint_get_fields(
      c("pre", "post", "downstream","upstream"),
      conn = conn, dataset=dataset, ...)

    connections <-lapply(ll$roiInfo[hasroi],
                         function(x) extract_connectivity_df(
                           rois=rois, json=x, postFix = roiInfoFields))
    resultsD <- cbind(ll[hasroi, 1, drop=FALSE], dplyr::bind_rows(connections))
    if (!full) {
      results <-
        matrix(
          nrow = length(rois),
          ncol = length(rois),
          dimnames = list(inputs = rois, outputs = rois)
        )
      if (statistic == "count") {
        for (inp in rois) {
          for (out in rois) {
            results[inp, out] <-
              length(which(resultsD[[paste0(inp, ".post")]] > 0 &
                             resultsD[[paste0(out, ".pre")]] > 0))
          }
        }
      } else{
        totalInputs <- neuprint_get_meta(resultsD$bodyid)$post
        for (inp in rois) {
          for (out in rois) {
            results[inp, out] <-
              sum((resultsD[[paste0(out, ".pre")]] * resultsD[[paste0(inp, ".post")]] /
                     totalInputs)[totalInputs > 0])
          }
        }
      }
    } else {
      results <- resultsD
    }
  }
  results
}

#' @title Download a region of interest as a 3D mesh
#'
#' @param roi region of interest for a dataset
#'@param roi_check default TRUE. If FALSE, and roi isn't null, ignore ROI checking step
#' @inheritParams neuprint_fetch_custom
#' @param ... methods passed to \code{\link{neuprint_login}}
#' @export
#' @return a \code{\link[rgl]{mesh3d}} object
#' @seealso \code{\link{neuprint_ROIs}}
neuprint_ROI_mesh <- function(roi, roi_check=TRUE, dataset = NULL, conn = NULL, ...){
  conn=neuprint_login(conn)
  dataset = check_dataset(dataset, conn=conn)
  if (roi_check) roicheck = neuprint_check_roi(rois=roi, dataset = dataset, conn = conn, ...)
  roiQuery = neuprint_fetch(path=paste("api/roimeshes/mesh", dataset, roi,
                                       sep="/"),
                            parse.json = FALSE,
                            include_headers = FALSE)
  tf = tempfile()
  on.exit(unlink(tf))
  writeLines(httr::content(roiQuery, as="text", encoding = "UTF-8"), tf)
  readobj::read.obj(tf,convert.rgl=TRUE)[[1]]
}

# hidden
neuprint_check_roi <- function(rois, superLevel = NULL, dataset = NULL, conn = NULL, ...){
  possible.rois = neuprint_ROIs(dataset=dataset,conn=conn, superLevel = superLevel, ...)
  if(!all(rois%in%possible.rois)){
    stop("Regions of interest provided that are not demarcated in dataset ", dataset, " for server ", neuprint_login(conn)$server,
         ". Please call neuprint_ROIs(superLevel = NULL) to see the available ROIs.")
  }else{
    TRUE
  }
}
