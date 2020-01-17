#' @title Find bodies within a given ROI
#'
#' @description  Find bodies that input/output within certain ROIs (i.e. they must have synapses in these areas)
#' @param input_ROIs a vector of input ROIs. Use \code{neuprint_ROIs} to see what is available.
#' @param output_ROIs a vector of input ROIs. Use \code{neuprint_ROIs} to see what is available.
#' @param roi a single ROI. Use \code{neuprint_ROIs} to see what is available.
#' @param statuses if not NULL, only bodies with the given status are considered. Statuses include:
#' Unimportant,0.5assign,Leaves,Prelim Roughly Traced, Anchor, Orphan.
#' @param all_segments if TRUE, all bodies are considered, if FALSE, only 'Neurons', i.e. bodies with a status roughly traced status.
#' @param dataset optional, a dataset you want to query. If NULL, the default specified by your R environ file is used. See \code{neuprint_login} for details.
#' @param conn optional, a neuprintr connection object, which also specifies the neuPrint server see \code{?neuprint_login}.
#' If NULL, your defaults set in your R.profile or R.environ are used.
#' @param ... methods passed to \code{neuprint_login}
#' @return a n x n matrix, where the rows are input neurons and the columns are their targets
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_simple_connectivity}}, \code{\link{neuprint_common_connectivity}}
#' @export
#' @rdname neuprint_find_neurons
neuprint_find_neurons <- function(input_ROIs,
                                  output_ROIs = NULL,
                                  statuses = NULL,
                                  dataset = NULL,
                                  conn = NULL,
                                  all_segments = TRUE,
                                  ...){
  dataset <- check_dataset(dataset)
  if(!is.null(statuses)){
    possible.statuses = c("Unimportant","0.5assign","Leaves","Prelim Roughly Traced", "Anchor", "Orphan")
    if(sum(!statuses%in%possible.statuses)){
      stop("Invalid stauses provided. Statuses must be NULL to accept any body status, or on of: ", possible.statuses)
    }
  }
  all_segments = ifelse(all_segments,"true","false")
  roicheck = neuprint_check_roi(rois=unique(c(input_ROIs,output_ROIs)), dataset = dataset, conn = conn, ...)
  Payload = noquote(sprintf('{"dataset":"%s","input_ROIs":%s,"output_ROIs":%s,"statuses":%s,"all_segments":%s}',
                            dataset, jsonlite::toJSON(input_ROIs),
                            jsonlite::toJSON(output_ROIs),
                            ifelse(is.null(statuses),jsonlite::toJSON(list()),jsonlite::toJSON(statuses)),
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
  innervation = lapply(found.neurons[[2]], function(f)
    extract_connectivity_df(rois = c(input_ROIs,output_ROIs),
                            json=unlist(f[columns=="roiInfo"])))
  innervation = do.call(rbind,innervation)
  cbind(neurons,innervation)
}

#' @export
#' @rdname neuprint_find_neurons
neuprint_bodies_in_ROI <- function(roi, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  dataset <- check_dataset(dataset)
  conn=neuprint_login(conn)
  dp=neuprint_dataset_prefix(dataset, conn=conn)
  all_segments = ifelse(all_segments,"Segment","Neuron")
  roicheck = neuprint_check_roi(rois=roi, dataset = dataset, conn = conn, ...)
  cypher = sprintf("MATCH (n :`%s`) WHERE n.%s WITH n AS n, apoc.convert.fromJsonMap(n.roiInfo) AS roiInfo RETURN n.bodyId AS bodyid, n.size AS voxels, n.pre, n.post, roiInfo.%s.pre, roiInfo.%s.post",
                paste0(dp, all_segments),
                roi,
                roi,
                roi)
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, ...)
  d = data.frame(do.call(rbind,lapply(nc$data,unlist)))
  colnames(d) = unlist(nc$columns)
  d
}

#' @title Get the connectivity between ROIs in a neuPrint dataset
#'
#' @param rois regions of interest for a dataset
#' @param cached pull cached results (TRUE) or recalculate the connectivity (FALSE)?
#' @param full return all neurons involved (TRUE, the default) or give a ROI summary (FALSE, default behavior if `cached` is TRUE)
#' @param statistic either "weight" or count" (default "weight"). Which number to return (see neuprint explorer for details) for summary results (either `full` is FALSE or `cached` is TRUE)
#' @param dataset optional, a dataset you want to query. If NULL, the default specified by your R environ file is used. See \code{neuprint_login} for details.
#' @param conn optional, a neuprintr connection object, which also specifies the neuPrint server see \code{?neuprint_login}.
#' If NULL, your defaults set in your R.profile or R.environ are used.
#' @param ... methods passed to \code{neuprint_login}
#' @seealso \code{\link{neuprint_simple_connectivity}}, \code{\link{neuprint_common_connectivity}}
#' @export
#' @rdname neuprint_ROI_connectivity
neuprint_ROI_connectivity <- function(rois, cached = FALSE, full=TRUE, statistic = c("weight","count"),dataset = NULL, conn = NULL, ...){
  statistic <- match.arg(statistic)
  dataset <- check_dataset(dataset)
  roicheck <- neuprint_check_roi(rois=rois, dataset = dataset, conn = conn, ...)

  if (cached){
    results <-matrix(nrow=length(rois),ncol=length(rois),dimnames = list(inputs=rois,outputs=rois))
    roi.conn = neuprint_fetch(path = 'api/cached/roiconnectivity', conn = conn, ...)
    for (inp in rois){
      for (out in rois){
         results[inp,out] <-  roi.conn$weights[[paste(inp,out,sep="=>")]][[statistic]]
      }
    }
  }else{
    Payload = noquote(sprintf('{"dataset":"%s","rois":%s}',
                            dataset,
                            ifelse(is.null(rois),jsonlite::toJSON(list()),jsonlite::toJSON(rois))))
    class(Payload) = "json"
    roi.conn <- neuprint_fetch(path = 'api/npexplorer/roiconnectivity', body = Payload, conn = conn, ...)
    connData <- roi.conn$data[sapply(roi.conn$data,function(d) any(sapply(rois,function(r) grepl(paste0("\"",r,"\""), d[[2]]))))]
    connections <-lapply(connData, function(rc) extract_connectivity_df(rois=rois,json=rc[[2]]))
    resultsD <- dplyr::bind_rows(connections)
    resultsD$bodyid <- as.character(sapply(connData, function(d) d[[1]]))
    if (!full){
      results <- matrix(nrow=length(rois),ncol=length(rois),dimnames = list(inputs=rois,outputs=rois))
      if (statistic == "count"){
        for (inp in rois){
          for (out in rois){
            results[inp,out] <- length(which(resultsD[[paste0(inp,".post")]]>0 & resultsD[[paste0(out,".pre")]]>0))
          }
        }
      }else{
        totalInputs <- neuprint_get_meta(resultsD$bodyid)$post
        for (inp in rois){
          for (out in rois){
            results[inp,out] <- sum((resultsD[[paste0(out,".pre")]]*resultsD[[paste0(inp,".post")]]/totalInputs)[totalInputs>0])
          }
        }
      }
    }else{
      results <- resultsD
    }
  }

  results
}

#' @title Import a region of interest as a mesh
#'
#' @param roi region of interest for a dataset
#' @param dataset optional, a dataset you want to query. If NULL, the default specified by your R environ file is used. See \code{neuprint_login} for details.
#' @param conn optional, a neuprintr connection object, which also specifies the neuPrint server see \code{?neuprint_login}.
#' If NULL, your defaults set in your R.profile or R.environ are used.
#' @param ... methods passed to \code{neuprint_login}
#' @export
#' @rdname neuprint_ROI_mesh
neuprint_ROI_mesh <- function(roi, dataset = NULL, conn = NULL, ...){
  dataset <- check_dataset(dataset)
  roicheck = neuprint_check_roi(rois=roi, dataset = dataset, conn = conn, ...)
  roiQuery = neuprint_fetch(path=paste("api/roimeshes/mesh",dataset,roi,sep="/"),parse.json = FALSE,include_headers = FALSE)
  tf = tempfile()
  writeLines(httr::content(roiQuery,as="text"),tf)
  readobj::read.obj(tf,convert.rgl=TRUE)[[1]]
}

# hidden
neuprint_check_roi <- function(rois, dataset = NULL, conn = NULL, ...){
  possible.rois = neuprint_ROIs(dataset=dataset,conn=conn, ...)
  if(!all(rois%in%possible.rois)){
    stop("Regions of interest provided that are not demarcated in dataset ", dataset, " for server ", neuprint_login(conn)$server,
         ". Please call neuprint_ROIs() to see the available ROIs.")
  }else{
    TRUE
  }
}
