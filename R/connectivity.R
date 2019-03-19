#' @title Get an matrix of connectivities between neurons
#'
#' @description  Get an adjacency matrix for the synaptic connectivity within a set of specified bodies
#' @param bodyids the body IDs for neurons/segments (bodies) you wish to query
#' @param dataset optional, a dataset you want to query. If NULL, the default specified by your R environ file is used. See \code{neuprint_login} for details.
#' @param conn optional, a neuprintr connection object, which also specifies the neuPrint server see \code{?neuprint_login}.
#' If NULL, your defaults set in your R.profile or R.environ are used.
#' @param ... methods passed to \code{neuprint_login}
#' @return a n x n matrix, where the rows are input neurons and the columns are their targets
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_simple_connectivity}}, \code{\link{neuprint_common_connectivity}}
#' @export
#' @rdname neuprint_get_adjacency_matrix
neuprint_get_adjacency_matrix <- function(bodyids, dataset = NULL, conn = NULL, ...){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  cypher = sprintf("WITH %s AS input MATCH (n:`%s-Neuron`)-[c:ConnectsTo]->(m) WHERE n.bodyId IN input AND m.bodyId IN input RETURN n.bodyId AS upstream, m.bodyId AS downstream, c.weight AS weight, n.name AS upName, m.name AS downName",
                   dataset,
                   jsonlite::toJSON(bodyids))
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, ...)
  m = matrix(0,nrow = length(bodyids),ncol = length(bodyids))
  rownames(m) = bodyids
  for(i in 1:length(nc$data)){
    s = unlist(nc$data[[1]])
    m[as.character(s[1]),as.character(s[2])] = as.numeric(s[3])
  }
  m = t(apply(m,1,as.numeric))
  colnames(m) = bodyids
  m
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
#'   body ids
#' @seealso \code{\link{neuprint_common_connectivity}},
#'   \code{\link{neuprint_get_adjacency_matrix}}
#' @export
#' @rdname neuprint_simple_connectivity
neuprint_simple_connectivity <- function(bodyids,
                                         prepost = c("PRE","POST"),
                                         dataset = NULL, conn = NULL, ...){
  if(is.null(dataset)){ # Get a default dataset if none specified
    dataset = unlist(getenvoroption("dataset"))
  }
  find_inputs = ifelse(prepost=="PRE", "false","true")
  Payload = noquote(sprintf('{"dataset":"%s","neuron_ids":%s,"find_inputs":%s}',
                            dataset,
                            jsonlite::toJSON(bodyids),
                            find_inputs))
  class(Payload) = "json"
  simp.conn = neuprint_fetch(path = 'api/npexplorer/commonconnectivity', body = Payload, conn = conn, ...)
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
    b = a[grepl(sprintf("\"%s\"",roi),a)]
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


