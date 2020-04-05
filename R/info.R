#' @title Find out what some information about your neuPrint server
#'
#' @description  Get summary information about the datasets hosted by the
#'   neuPrint server in which you are interested
#' @param ... methods passed to \code{neuprint_login}
#' @inheritParams neuprint_fetch_custom
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

# memoised version
# nb this will timeout after 1h, which seems a reasonable trade-off
neuprint_datasets_memo <- memoise::memoise(neuprint_datasets, ~memoise::timeout(3600))

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
#' @param superLevel whether not to show 'superlevel' ROIs - ROIs composed of
#'   other ROIs. If set to NULL, both low-level and superlevel ROIs are
#'   returned.
#' @param fromNeuronFields : Whether to use fields of :Neuron nodes to fetch the
#'   ROIs. If FALSE (the default), use the dataset metadata instead. If TRUE,
#'   the \code{superLevel} argument is ignored
#' @param ... methods passed to \code{neuprint_login}
#' @inheritParams neuprint_fetch_custom
#' @seealso \code{\link{neuprint_login}}, \code{\link{neuprint_datasets}},
#'   \code{\link{neuprint_ROI_hierarchy}}
#' @export
neuprint_ROIs <- function(superLevel = FALSE, dataset = NULL, fromNeuronFields= FALSE,conn = NULL, ...){
  if (fromNeuronFields){
    rois <- neuprint_get_fields(possibleFields = c("bodyId", "pre", "post",
                                                   "upstream", "downstream",
                                                   "status", "statusLabel",
                                                   "cropped", "instance", "name",
                                                   "size", "type", "cellBodyFiber",
                                                   "somaLocation", "somaRadius","roiInfo"),
                                limit=200,
                                negateFields=TRUE,
                                dataset = NULL, conn = NULL, ...)
  }else{
  ds = neuprint_datasets(conn=conn, ...)
  conn=neuprint_login(conn)
  dataset = check_dataset(dataset, conn=conn)
  if(is.null(superLevel)){
    rois = c(ds[[dataset]]$superLevelROIs,ds[[dataset]]$ROIs)
    if(is.null(rois)){
      rois = c(ds[[1]]$superLevelROIs,ds[[1]]$ROIs)
    }
  }else if(superLevel){
    rois = ds[[dataset]]$superLevelROIs
    if(is.null(rois)){
      rois = ds[[1]]$superLevelROIs
    }
  }else{
    rois = ds[[dataset]]$ROIs
    if(is.null(rois)){
      rois = ds[[1]]$ROIs
    }
  }}
  sort(rois)
}


#' @title Get the region of interest (ROI) hierarchy in a dataset
#'
#' @param root Character vector specifying a root node that defines a subgraph
#'   of the ROI hierarchy. The default (\code{root=NULL}) will return the whole
#'   hierarchy.
#' @param rval Whether to return an edge list \code{data.frame} (the default) or
#'   an \code{\link{igraph}} object.
#' @param ... additional arguments passed to \code{\link{neuprint_fetch_custom}}
#' @inheritParams neuprint_fetch_custom
#' @examples
#' \donttest{
#' g = neuprint_ROI_hierarchy(rval='graph')
#' igraph::plot.igraph(g, layout=igraph::layout_as_tree)
#'
#' # just the Mushroom Body Nodes on right hand side
#' mbg=neuprint_ROI_hierarchy(root='MB(R)', rval='graph')
#' igraph::plot.igraph(mbg)
#' # find just the terminal nodes of the graph
#' mbterms=nat::endpoints(mbg)
#' mbterms
#'
#' \dontrun{
#' # read in all the MB meshes
#' mbmeshes=sapply(mbterms, neuprint_ROI_mesh, simplify = F)
#' # 3D plot in different colours
#' mapply(shade3d, mbmeshes, col=rainbow(length(mbmeshes)))
#' }
#' }
#' @seealso \code{\link{neuprint_ROIs}}, \code{\link{neuprint_get_roiInfo}}
#' @export
neuprint_ROI_hierarchy <- function(root=NULL, rval=c("edgelist","graph"),
                                   dataset = NULL, conn = NULL,...){
  conn=neuprint_login(conn)
  dataset = check_dataset(dataset, conn=conn)
  rval=match.arg(rval)
  cypher = sprintf("MATCH (m:Meta) WITH m as m, apoc.convert.fromJsonMap(m.roiHierarchy) as roiHierarchy RETURN roiHierarchy")
  nc = neuprint_fetch_custom(cypher=cypher, dataset = dataset, conn=conn, ...)
  roi.edgelist = data.frame()
  addin <- function(x, parent){
    y = unlist(x, recursive = FALSE)
    nams = names(y)
    if(is.null(nams)){
      addin(x=y, parent = parent)
    }else{
      roi = grepl("name",nams)
      children = (grepl("children", nams)+!roi)>1
      if(length(roi)&sum(roi)>0){
        e = data.frame(parent = parent, roi = unlist(y[which(roi)]))
        roi.edgelist ->> roi.edgelist
        roi.edgelist <<- rbind(roi.edgelist, e)
      }
      if(length(children)&sum(children)>0){
        for(child in which(children)){
          addin(x = y[which(children)][child],
                parent = ifelse(sum(roi)>0,unlist(y[which(roi)]),parent)
                )
        }
      }
    }
  }
  addin(nc$data, parent = dataset)
  rownames(roi.edgelist) = NULL
  el=roi.edgelist
  g=igraph::graph_from_data_frame(el, directed = TRUE)
  if(!is.null(root)) {
    if(isFALSE(root%in%names(igraph::V(g))))
      stop("Requested root does not seem to be a valid node of ROI hierarchy!")
    selnodes=na.omit(igraph::dfs(g, root, unreachable = F)$order)
    g <- igraph::induced_subgraph(g, selnodes)
    el <- as.data.frame(igraph::as_edgelist(g))
    colnames(el)=colnames(roi.edgelist)
  }

  if(rval=='graph') g else el
}



