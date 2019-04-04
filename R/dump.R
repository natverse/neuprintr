#' @title Download data from neuprint for specified bodyids / a specified ROI
#'
#' @description  Download neuron morphology and connectivity data to a specified directrory as .csv and .rda files
#' @inheritParams neuprint_read_neurons
#' @inheritParams neuprint_bodies_in_ROI
#' @inheritParams neuprint_connection_table
#' @inheritParams drvid::dv_get_voxels
#' @param dir the directory to which to save the dump
#' @param preprocess a function that can be applied to a nat::neuronlist object, to be called on neurons once they are read from
#' the neuprint server using \code{neuprint_read_neurons}
#' @param connectivity whether or not to include connectivity information in the dump, i.e. an adjacency matrix between bodyids
#' @param volumes whether or not to include neuron volume information in the dump, i.e. voxels. Currently only  works by talking to a DVID server using
#' the package drvid
#' @param voxel.thresh the size threshold, in number of voxels, a neuron/segment must exceed, to be included in the dump, if read from an ROI
#' @seealso \code{\link{neuprint_get_synapses}}, \code{\link{neuprint_read_neurons}}
#' @export
#' @rdname neuprint_dump
#' @importFrom nat write.neurons
neuprint_dump <- function(dir, bodyids = NULL, roi = NULL, preprocess = NULL, connectivity = TRUE, volumes = TRUE,
                          meta = TRUE, nat = TRUE, drvid = TRUE, flow.centrality = FALSE, soma = TRUE, estimate.soma = FALSE,
                          heal = TRUE, connectors = TRUE, all_segments = TRUE, resample = FALSE,
                          scale = 4, voxel.thresh = 1e+07, split = c("postsynapses","presynapses","distance"),
                          dataset = NULL, conn=NULL, OmitFailures = TRUE, ...){
  message("making data dump in directory ", dir)
  conn = neuprint_login(conn)
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  if(is.null(roi)&is.null(bodyids)){
    stop("You must provide either a vector of bodyids or an ROI for your dataset, in order to select neurons to dump at location ", dir,
         " If both are provided, extra bodyids from within the ROI will be added to those in argument bodyids")
  }
  if(!is.null(roi)){ # get bodids in ROI
    message("fetching bodyids in ROI ", roi, " in ", dataset)
    inroi = neuprint_bodies_in_ROI( roi = roi,
                                    dataset = dataset, conn=conn, ...)
    inroi = subset(inroi, voxels>voxel.thresh)
    bodyids = as.numeric(unique(c(unlist(inroi$bodyid),bodyids)))
  }
  # Fetch neuron data
  message("Reading neurons from ", conn$server, " for dataset: ", dataset)
  neurons = neuprint_read_neurons(bodyids = bodyids, meta = meta, nat = nat, drvid=drvid, flow.centrality = flow.centrality, soma = soma, heal = heal, connectors = connectors,
                                  all_segments = all_segments, dataset = dataset, resample = resample, split = split,
                                  conn = conn, OmitFailures = OmitFailures, ...)
  # pre-process data
  if(!is.null(preprocess)){
    message("processing neuronlist data")
    neurons = preprocess(neurons)
  }
  # save neuronlist data
  dir.create(file.path(dir, "neuronlist"), showWarnings = FALSE)
  save(neurons,file=paste0(dir,"/neuronlist/neuronlist.rda"))
  utils::write.csv(neurons[,], file = paste0(dir,"/neuronlist/neuronlist_meta_data.csv"))
  # save SWC files
  message("saving SWC files")
  dir.create(file.path(dir, "swc"), showWarnings = FALSE)
  write.neurons(neurons,dir = file.path(dir, "swc"), format = "swc", files = paste0(bodyid,".swc"), Force = TRUE)
  # save synapse locations
  if(connectors){
    message("saving synapse locations")
    dir.create(file.path(dir, "connectors"), showWarnings = FALSE)
    pb <- utils::txtProgressBar(min = 0, max = length(neurons), style = 3)
    for(n in 1:length(neurons)){
      utils::write.csv(neurons[[n]]$connectors,file=paste0(dir,"/connectors/",names(neurons)[n],"_connectors.swc"))
      utils::setTxtProgressBar(pb, n)
    }
    close(pb)
  }
  # save connectivity between the body ids
  if(connectivity){
    message("saving adjacency matrix")
    dir.create(file.path(dir, "connectivity"), showWarnings = FALSE)
    adjm = neuprint_get_adjacency_matrix(bodyids = bodyids, dataset = dataset, all_segments = all_segments,
                                         conn = conn, ...)
    message("saving upstream connection table")
    pre = neuprint_connection_table(bodyids = bodyids, prepost = "PRE", progress = TRUE, roi = NULL,
                                    dataset = dataset, conn = conn, all_segments = all_segments, ... )
    message("saving downstream connection table")
    post = neuprint_connection_table(bodyids = bodyids, prepost = "POST", progress = TRUE, roi = NULL,
                                    dataset = dataset, conn = conn, all_segments = all_segments, ... )
    utils::write.csv(adjm,paste0(dir,"/connectivity/adjacency_matrix.csv"))
    utils::write.csv(pre,paste0(dir,"/connectivity/pre_connection_table.csv"))
    utils::write.csv(post,paste0(dir,"/connectivity/post_connection_table.csv"))
  }
  # save volumes
  if(volumes&drvid){
    message("saving voxels")
    dir.create(file.path(dir, "voxels", scale), showWarnings = FALSE, recursive = TRUE)
    voxels = pbapply::pblapply(bodyids, drvid::dv_get_voxels, scale = scale, conn = NULL, ...)
    pb <- utils::txtProgressBar(min = 0, max = length(bodyids), style = 3)
    for(v in 1:length(voxels)){
      utils::write.csv(voxels[[v]],file=file.path(dir, "voxels", scale, paste0(bodyids[v],"_voxels.csv")))
      utils::setTxtProgressBar(pb, v)
    }
    close(pb)
  }
  message("done!")
}
