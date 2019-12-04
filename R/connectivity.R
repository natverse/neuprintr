#' @title Get a matrix of connectivities between bodies
#'
#' @description  Get an adjacency matrix for the synaptic connectivity within a set of specified bodies
#' @inheritParams neuprint_read_neurons
#' @return a n x n matrix, where the rows are input neurons and the columns are their targets
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_simple_connectivity}}, \code{\link{neuprint_common_connectivity}}
#' @export
#' @rdname neuprint_get_adjacency_matrix
neuprint_get_adjacency_matrix <- function(bodyids, dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  all_segments.json = ifelse(all_segments,"Segment","Neuron")
  cypher = sprintf("WITH %s AS input MATCH (n:`%s-%s`)-[c:ConnectsTo]->(m) WHERE n.bodyId IN input AND m.bodyId IN input RETURN n.bodyId AS upstream, m.bodyId AS downstream, c.weight AS weight, n.name AS upName, m.name AS downName",
                   jsonlite::toJSON(as.numeric(unique(unlist(bodyids)))),
                   dataset,
                   all_segments.json)
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, ...)
  m = matrix(0,nrow = length(bodyids),ncol = length(bodyids))
  rownames(m) = colnames(m) = bodyids
  for(i in 1:length(nc$data)){
    s = unlist(nc$data[[1]])
    m[as.character(s[1]),as.character(s[2])] = as.numeric(s[3])
  }
  m
}

#' @title Get the upstream and downstream connectivity of a neuron
#'
#' @description Get the upstream and downstream connectivity of a body, restricted to within an ROI if specified
#' @inheritParams neuprint_read_neurons
#' @inheritParams neuprint_find_neurons
#' @param prepost whether to look for partners presynaptic to postsynaptic to the given bodyids
#' @param progress default FALSE. If TRUE, the API is called separately for each neuron and yuo can asses its progress, if an error is thrown by any one bodyid, that bodyid is ignored
#' @return a data frame giving partners within an ROI, the connection strength for weights to or from that partner, and the direction, for the given bodyid
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_simple_connectivity}}, \code{\link{neuprint_common_connectivity}}
#' @export
#' @rdname neuprint_connection_table
neuprint_connection_table <- function(bodyids, prepost = c("PRE","POST"), roi = NULL, progress = FALSE,
                                      dataset = NULL, all_segments = TRUE, conn = NULL, ...){
  prepost = match.arg(prepost)
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  all_segments.json = ifelse(all_segments,"Segment","Neuron")
  if(!is.null(roi)){
    roicheck = neuprint_check_roi(rois=roi, dataset = dataset, conn = conn, ...)
  }
  if(progress){
    d  = do.call(rbind, pbapply::pblapply(bodyids, function(bi) tryCatch(neuprint_connection_table(
      bodyids = bi,
      prepost = prepost,
      roi = roi,
      progress = FALSE,
      dataset = dataset, conn = conn, ...),
      error = function(e) NULL)))
    return(d)
  }
  cypher = sprintf("WITH %s AS bodyIds UNWIND bodyIds AS bodyId MATCH (a:`%s-%s`)<-[:From]-(c:ConnectionSet)-[:To]->(b:`%s-%s`), (c)-[:Contains]->(s:Synapse) WHERE %s (s.type='post') AND %s.bodyId=bodyId RETURN a.bodyId AS %s, b.bodyId AS %s, count(*) AS weight",
                   jsonlite::toJSON(unique(as.numeric(unlist(bodyids)))),
                   dataset,
                   all_segments.json,
                   dataset,
                   all_segments.json,
                   ifelse(is.null(roi),"",sprintf("(exists(s.`%s`)) AND",roi)),
                   ifelse(prepost=="POST","b","a"),
                   ifelse(prepost=="POST","partner","bodyid"),
                   ifelse(prepost=="POST","bodyid","partner"))
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn)
  d = data.frame(do.call(rbind,lapply(nc$data,unlist)))
  colnames(d) = unlist(nc$columns)
  d$prepost = ifelse(prepost=="PRE",0,1)
  d = d[order(d$weight,decreasing=TRUE),]
  d[,c("bodyid", "partner", "weight", "prepost")]
}

#' @title Get the common synaptic partners for a set of neurons
#'
#' @description  Get the neurons that are shared synaptic partners for a set of given bodyids, either upstream or downstream.
#' @param bodyids the cypher by which to make your search
#' @param statuses if not NULL, only bodies with the given status are considered. Statuses include:
#' Unimportant,0.5assign,Leaves,Prelim Roughly Traced, Anchor, Orphan.
#' @param prepost whether to look for partners presynaptic to postsynaptic to the given bodyids
#' @param all_segments if TRUE, all bodies are considered, if FALSE, only 'Neurons', i.e. bodies with a status roughly traced status.
#' @param dataset optional, a dataset you want to query. If NULL, the default specified by your R environ file is used. See \code{neuprint_login} for details.
#' @param conn optional, a neuprintr connection object, which also specifies the neuPrint server see \code{?neuprint_login}.
#' If NULL, your defaults set in your R.profile or R.environ are used.
#' @param ... methods passed to \code{neuprint_login}
#' @return a n x m matrix where n correspond to the neurons that all connect to m bodyids
#' @seealso \code{\link{neuprint_simple_connectivity}}, \code{\link{neuprint_get_adjacency_matrix}}
#' @export
#' @rdname neuprint_common_connectivity
neuprint_common_connectivity <- function(bodyids, statuses = NULL,
                                prepost = c("PRE","POST"),
                                all_segments = TRUE,
                                dataset = NULL, conn = NULL, ...){
  prepost = match.arg(prepost)
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  if(!is.null(statuses)){
    possible.statuses = c("Unimportant","0.5assign","Leaves","Prelim Roughly Traced", "Anchor", "Orphan")
    if(sum(!statuses%in%possible.statuses)){
      stop("Invalid stauses provided. Statuses must be NULL to accept any body status, or on of: ", possible.statuses)
    }
  }
  find_inputs = ifelse(prepost=="PRE", "false","true")
  all_segments = ifelse(all_segments,"true","false")
  Payload = noquote(sprintf('{"dataset":"%s","neuron_ids":%s,"statuses":%s,"find_inputs":%s,"all_segments":%s}',
                            dataset,
                            jsonlite::toJSON(bodyids),
                            ifelse(is.null(statuses),jsonlite::toJSON(list()),jsonlite::toJSON(statuses)),
                            find_inputs,
                            all_segments))
  class(Payload) = "json"
  com.conn = neuprint_fetch(path = 'api/npexplorer/commonconnectivity', body = Payload, conn = conn, ...)
  m = matrix(0,nrow = length(bodyids),ncol = length(com.conn$data[[1]][[1]]))
  rownames(m) = paste0(bodyids,"_weight")
  connected = c()
  for(i in 1:length(com.conn$data[[1]][[1]])){
    s = com.conn$data[[1]][[1]][[i]]
    find = match(names(s),rownames(m))
    add = find[!is.na(find)]
    m[add,i] = unlist(s)[!is.na(find)]
    connected = c(connected,ifelse(is.null(s$input),s$output,s$input))
  }
  m = t(apply(m,1,as.numeric))
  colnames(m) = connected
  rownames(m) = bodyids
  m = m[,apply(m,2,function(x) sum(x==0)==0)]
  m
}

#' @title Get a list of connected partners for a neuron
#'
#' @description  Get all of the neurons in the database that connect to the
#'   query neurons, either upstream or downstream of them
#' @param bodyids the cypher by which to make your search
#' @param prepost whether to look for partners presynaptic to postsynaptic to
#'   the given bodyids
#' @param dataset optional, a dataset you want to query. If NULL, the default
#'   specified by your R environ file is used. See \code{neuprint_login} for
#'   details.
#' @param conn optional, a neuprintr connection object, which also specifies the
#'   neuPrint server see \code{\link{neuprint_login}}. If NULL, your defaults
#'   set in your R.profile or R.environ are used.
#' @param ... methods passed to \code{neuprint_login}
#' @return a n x m matrix where n correspond to the neurons that connect to m
#'   bodyids
#' @seealso \code{\link{neuprint_common_connectivity}},
#'   \code{\link{neuprint_get_adjacency_matrix}}
#' @export
#' @rdname neuprint_simple_connectivity
neuprint_simple_connectivity <- function(bodyids,
                                         prepost = c("PRE","POST"),
                                         dataset = NULL, conn = NULL, ...){
  prepost = match.arg(prepost)
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  find_inputs = ifelse(prepost=="PRE", "false","true")
  if(length(bodyids)>10){
    m  = do.call(rbind, pbapply::pblapply(bodyids, function(bi) tryCatch(neuprint_simple_connectivity(
                                bodyids = bi,
                                prepost = prepost,
                                dataset = dataset, conn = conn, ...),
                                error = function(e) NULL)))
    return(m)
  }
  Payload = noquote(sprintf('{"dataset":"%s","neuron_ids":%s,"find_inputs":%s}',
                            dataset,
                            jsonlite::toJSON(bodyids),
                            find_inputs))
  class(Payload) = "json"
  simp.conn = neuprint_fetch(path = 'api/npexplorer/commonconnectivity', body = Payload, conn = conn, ...)
  if(!length(simp.conn$data)){
    return(NULL)
  }
  m = matrix(0,nrow = length(bodyids),ncol = length(simp.conn$data[[1]][[1]]))
  rownames(m) = paste0(bodyids,"_weight")
  connected = c()
  for(i in 1:length(simp.conn$data[[1]][[1]])){
   s = simp.conn$data[[1]][[1]][[i]]
   find = match(names(s),rownames(m))
   add = find[!is.na(find)]
   m[add,i] = unlist(s)[!is.na(find)]
   connected = c(connected,ifelse(is.null(s$input),s$output,s$input))
  }
  m = apply(m,1,as.numeric)
  rownames(m) = connected
  colnames(m) = bodyids
  #m = reshape2::melt(m)
  #colnames(m) = c("partner","bodyid","weight")
  #m$prepost = ifelse(prepost=="PRE",0,1)
  m
}

# hidden, caution, does not deal with left/right neuropils
extract_connectivity_df <- function(rois, json){
  if(is.null(json)){
    return(NULL)
  }
  a = unlist(strsplit(json,"}"))
  values = data.frame()
  for(roi in rois){
    b = a[grepl(sprintf("\"%s\"",roi),a,fixed=TRUE)]
    if(length(b)){
      c = unlist(strsplit(b,","))
      n = as.numeric(gsub("[^0-9.]", "", c))
      n = n[!is.na(n)]
    }else{
      n = c(0,0)
    }
    d = data.frame(n)
    rownames(d) = paste0(roi,c("_pre","_post"))
    values = rbind(values,d)
  }
  t(values)
}


