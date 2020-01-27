#' @title Get a matrix of connectivities between bodies
#'
#' @description  Get an adjacency matrix for the synaptic connectivity within a set of specified bodies
#' @inheritParams neuprint_read_neurons
#' @return a n x n matrix, where the rows are input neurons and the columns are their targets
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_simple_connectivity}}, \code{\link{neuprint_common_connectivity}}
#' @export
#' @rdname neuprint_get_adjacency_matrix
neuprint_get_adjacency_matrix <- function(bodyids, dataset = NULL, all_segments = FALSE, conn = NULL, ...){
  all_segments.json = ifelse(all_segments,"Segment","Neuron")
  namefield=neuprint_name_field(conn)
  cypher = sprintf(
    paste(
      "WITH %s AS input MATCH (n:`%s`)-[c:ConnectsTo]->(m)",
      "WHERE n.bodyId IN input AND m.bodyId IN input",
      "RETURN n.bodyId AS upstream, m.bodyId AS downstream, c.weight AS weight, n.%s AS upName, m.%s AS downName"
    ),
    id2json(bodyids, uniqueids = TRUE),
    all_segments.json,
    namefield,
    namefield
  )
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, dataset = dataset, ...)
  m = matrix(0,nrow = length(bodyids),ncol = length(bodyids))
  rownames(m) = colnames(m) = bodyids
  for(i in 1:length(nc$data)){
    s = unlist(nc$data[[i]])
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
#' @param progress default FALSE. If TRUE, the API is called separately for each neuron and you can assess its progress, if an error is thrown by any one \code{bodyid}, that \code{bodyid} is ignored
#' @return a data frame giving partners within a set of ROIs, the connection strength for weights to or from that partner, and the direction, for the given bodyid
#' @seealso \code{\link{neuprint_fetch_custom}}, \code{\link{neuprint_simple_connectivity}}, \code{\link{neuprint_common_connectivity}}
#' @export
#' @rdname neuprint_connection_table
#' @examples
#' \donttest{
#' neuprint_connection_table(c(818983130, 1796818119))
#' }
neuprint_connection_table <- function(bodyids, prepost = c("PRE","POST"), roi = NULL, progress = FALSE,
                                      dataset = NULL, all_segments = FALSE, conn = NULL, ...){
  prepost = match.arg(prepost)
  conn=neuprint_login(conn)
  all_segments.json = ifelse(all_segments,"Segment","Neuron")
  bodyids=unique(id2char(bodyids))
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
    d <-  d[order(d$weight,decreasing=TRUE),]
    rownames(d) <- NULL
    return(d)
  }
  cypher = sprintf(paste("WITH %s AS bodyIds UNWIND bodyIds AS bodyId",
                         "MATCH (a:`%s`)-[c:ConnectsTo]->(b:`%s`)",
                         "WHERE %s.bodyId=bodyId",
                         "UNWIND %s AS k",
                         "RETURN a.bodyId AS %s, b.bodyId AS %s, k AS roi,",
                         "apoc.convert.fromJsonMap(c.roiInfo)[k].post AS weight"),
                   id2json(bodyids),
                   all_segments.json,
                   all_segments.json,
                   ifelse(prepost=="POST","a","b"),
                   ifelse(is.null(roi),"keys(apoc.convert.fromJsonMap(c.roiInfo))",paste("['",paste(roi,collapse="','"),"']",sep="")),
                   ifelse(prepost=="POST","bodyid","partner"),
                   ifelse(prepost=="POST","partner","bodyid")
                  )
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, dataset = dataset, ...)
  ## Filter out the rare cases where PSDs and tbars are in different ROIs (hence post is null)
  nc$data <- nc$data[sapply(nc$data,function(x) !is.null(x[[4]]))]
  d <-  data.frame(do.call(rbind,lapply(nc$data,unlist)),stringsAsFactors = FALSE)
  colnames(d) <-  unlist(nc$columns)
  d$weight <- as.integer(d$weight)
  d$prepost <-  ifelse(prepost=="PRE",0,1)
  d <-  d[order(d$weight,decreasing=TRUE),]
  rownames(d) <- NULL
  d[,c("bodyid", "partner", "roi","weight", "prepost")]
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
                                all_segments = FALSE,
                                dataset = NULL, conn = NULL, ...){
  prepost = match.arg(prepost)
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
                            id2json(bodyids, uniqueids = TRUE),
                            ifelse(is.null(statuses),jsonlite::toJSON(list()),jsonlite::toJSON(statuses)),
                            find_inputs,
                            all_segments))
  class(Payload) = "json"
  com.conn = neuprint_fetch(path = 'api/npexplorer/commonconnectivity', body = Payload, conn = conn, ...)
  partnerType <- ifelse(prepost=="PRE","output","input")
  partnerNames <- sapply(com.conn$data[[1]][[1]],function(d) d[[partnerType]])
  partnerCount <- table(partnerNames)
  commonPartners <- names(partnerCount[partnerCount == length(bodyids)])
  m <-  matrix(0,nrow = length(bodyids),ncol = length(commonPartners))
  rownames(m) <-  paste0(bodyids,"_weight")
  colnames(m) <-  commonPartners
  comData <- com.conn$data[[1]][[1]][which(partnerNames %in% commonPartners)]

  for(s in comData){
    rName <- names(s)[names(s) %in% rownames(m)]
    m[rName,as.character(s[[partnerType]])] <- s[[rName]]
  }
  rownames(m) <-  bodyids
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
                                         dataset = NULL,
                                         conn = NULL,
                                         ...){
  prepost = match.arg(prepost)
  find_inputs = ifelse(prepost=="PRE", "false","true")
  # nb looks odd, but convert back to character to allow pblapply ...
  bodyids=unique(id2char(bodyids))
  if(length(bodyids)>10){
    m  = Reduce(function(x,y,...) dplyr::full_join(x,y,by=c("name",ifelse(prepost=="PRE","output","input"),"type")),(pbapply::pblapply(bodyids, function(bi) tryCatch(neuprint_simple_connectivity(
                                bodyids = bi,
                                prepost = prepost,
                                dataset = dataset, conn = conn, ...),
                                error = function(e) NULL))))
    return(m)
  }
  Payload = noquote(sprintf('{"dataset":"%s","neuron_ids":%s,"find_inputs":%s}',
                            dataset,
                            id2json(bodyids),
                            find_inputs))
  class(Payload) = "json"
  simp.conn = neuprint_fetch(path = 'api/npexplorer/commonconnectivity', body = Payload, conn = conn, simplifyVector = TRUE, ...)
  if(!length(simp.conn$data)){
    return(NULL)
  }
  d=simp.conn$data[[1]][[1]]
  weightcols <- grep("_weight$", names(d))
  othercols <- grep("_weight$", names(d), invert = T)
  partnercol=grep("put$", names(d))
  # m = matrix(0,nrow = length(bodyids),ncol = length(d))
  # rownames(m) = paste0(bodyids,"_weight")
  for(i in weightcols) {
    d[[i]][is.na(d[[i]])]=0
  }
  d=d[c(othercols, weightcols)]
  d
}

#' @title Get a list of paths of length n between 2 neurons
#'
#' @description  Get all of the paths in the database that connect the query
#'   neurons with at least \code{weightT} synapses at each step
#' @param body_pre the bodyid of the neuron at the start of the path
#' @param body_post the bodyid of the neuron at the end of the path
#' @param n the length of the path. If \code{n} is a vector, paths of length
#'   n[1] to n[2] are considered
#' @param weightT weight threshold
#' @param dataset optional, a dataset you want to query. If NULL, the default
#'   specified by your R environ file is used. See \code{neuprint_login} for
#'   details.
#' @param all_segments if TRUE, all bodies are considered, if FALSE, only
#'   'Neurons', i.e. bodies with a status roughly traced status.
#' @param conn optional, a neuprintr connection object, which also specifies the
#'   neuPrint server see \code{\link{neuprint_login}}. If NULL, your defaults
#'   set in your R.profile or R.environ are used.
#' @param roi Limit the search to connections happening within a certain ROI or
#'   set of ROIs (NULL by default)
#' @param ... methods passed to \code{neuprint_login}
#' @seealso \code{\link{neuprint_get_shortest_paths}},
#'   \code{\link{neuprint_common_connectivity}},
#'   \code{\link{neuprint_get_adjacency_matrix}}
#' @export
neuprint_get_paths <- function(body_pre, body_post, n, weightT=5, roi=NULL,
                               dataset = NULL, conn = NULL, all_segments=FALSE, ...){

  if (length(n)==1){
    n <- c(n,n)
  } else {
    # just to ensure min path comes first
    n=sort(n)
    if (length(n)!=2)
      stop("n must specify a single path length or a range of path lengths!")
  }
  if(min(n) < 1)
    stop("Minimum path length must be >=1!")

  conn <- neuprint_login(conn)

  if(!is.null(roi)){
    roicheck = neuprint_check_roi(rois=roi, dataset = dataset, conn = conn, ...)
    roi <- paste("AND (" ,paste0("exists(apoc.convert.fromJsonMap(x.roiInfo).`",roi,"`)",collapse=" OR "),")")
  }

  all_segments.json <-  ifelse(all_segments,"Segment","Neuron")

  cypher <-  sprintf(paste("WITH [%s,%s] AS bodies",
                           "UNWIND bodies[0] AS bodypre",
                           "UNWIND bodies[1] AS bodypost",
                           "MATCH p = (src : `%s`)-[ConnectsTo*%s..%s]->(dest:`%s`)",
                           "WHERE src.bodyId = bodypre AND dest.bodyId = bodypost AND",
                           "ALL (x in relationships(p) WHERE x.weight >= %s %s)",
                           "RETURN length(p) AS `length(path)`,[n in nodes(p) | [n.bodyId, n.instance, n.type]] AS path,[x in relationships(p) | x.weight] AS weights"
  ),
  id2json(body_pre, uniqueids = TRUE),
  id2json(body_post, uniqueids = TRUE),
  all_segments.json,
  n[1]-1,
  n[2],
  all_segments.json,
  weightT,
  ifelse(is.null(roi),"",roi)
  )

  nc <-  neuprint_fetch_custom(cypher=cypher, conn = conn, dataset = dataset, ...)

  connTable <- dplyr::bind_rows(lapply(nc$data, function(d){
                  l <- d[[1]]
                  tryCatch( dplyr::bind_rows(lapply(1:l, function(i){
                    data.frame(from=as.character(d[[2]][[i]][[1]]),
                               to=as.character(d[[2]][[i+1]][[1]]),
                               weight=d[[3]][[i]],
                               depth = i,
                               name.from=d[[2]][[i]][[2]],name.to=d[[2]][[i+1]][[2]],
                               type.from=d[[2]][[i]][[3]],type.to=d[[2]][[i+1]][[3]],
                               stringsAsFactors = FALSE)
                  })), error= function(e) NULL)
  }))
}

#' @title Get a list of the shortest paths between two neurons
#'
#' @description  Get all of the shortest paths in the database that connect the
#'   query neurons with at least weightT synapses at each step
#' @param body_pre the bodyid of the neuron at the start of the path
#' @param body_post the bodyid of the neuron at the end of the path
#' @param weightT weight threshold
#' @param dataset optional, a dataset you want to query. If NULL, the default
#'   specified by your R environ file is used. See \code{neuprint_login} for
#'   details.
#' @param roi Limit the search to connections happening within a certain ROI or
#'   set of ROIs (NULL by default)
#' @param all_segments if TRUE, all bodies are considered, if FALSE, only
#'   'Neurons', i.e. bodies with a status roughly traced status.
#' @param conn optional, a neuprintr connection object, which also specifies the
#'   neuPrint server see \code{\link{neuprint_login}}. If NULL, your defaults
#'   set in your R.profile or R.environ are used.
#' @param ... methods passed to \code{neuprint_login}
#' @seealso \code{\link{neuprint_get_paths}},
#'   \code{\link{neuprint_common_connectivity}},
#'   \code{\link{neuprint_get_adjacency_matrix}}
#' @export
neuprint_get_shortest_paths <- function(body_pre,body_post,weightT=5,roi=NULL,dataset = NULL, conn = NULL,all_segments=FALSE, ...){

  conn <- neuprint_login(conn)
  all_segments.json <-  ifelse(all_segments,"Segment","Neuron")

  if(!is.null(roi)){
    roicheck = neuprint_check_roi(rois=roi, dataset = dataset, conn = conn, ...)
    roi <- paste("AND (" ,paste0("exists(apoc.convert.fromJsonMap(x.roiInfo).`",roi,"`)",collapse=" OR "),")")
  }

  cypher <-  sprintf(paste("WITH [%s,%s] AS bodies",
                           "UNWIND bodies[0] AS bodypre",
                           "UNWIND bodies[1] AS bodypost",
                           "WITH * WHERE bodypre <> bodypost",
                           "MATCH p = allShortestPaths((src : `%s`)-[ConnectsTo*]->(dest:`%s`))",
                           "WHERE src.bodyId = bodypre AND dest.bodyId = bodypost AND",
                           "ALL (x in relationships(p) WHERE x.weight >= %s %s)",
                           "RETURN length(p) AS `length(path)`,[n in nodes(p) | [n.bodyId, n.instance, n.type]] AS path,[x in relationships(p) | x.weight] AS weights"
  ),
  id2json(body_pre, uniqueids = TRUE),
  id2json(body_post, uniqueids = TRUE),
  all_segments.json,
  all_segments.json,
  weightT,
  ifelse(is.null(roi),"",roi)
  )

  nc <-  neuprint_fetch_custom(cypher=cypher, conn = conn, dataset=dataset, ...)

  connTable <- dplyr::bind_rows(lapply(nc$data, function(d){
    l <- d[[1]]
    tryCatch( dplyr::bind_rows(lapply(1:l, function(i){
      data.frame(from=as.character(d[[2]][[i]][[1]]),
                 to=as.character(d[[2]][[i+1]][[1]]),
                 weight=d[[3]][[i]],
                 depth=i,
                 name.from=d[[2]][[i]][[2]],name.to=d[[2]][[i+1]][[2]],
                 type.from=d[[2]][[i]][[3]],type.to=d[[2]][[i+1]][[3]],
                 stringsAsFactors = FALSE)
    })), error = function(e) NULL)
  }))
}

# hidden, caution, does not deal with left/right neuropils
extract_connectivity_df <- function(rois, json){
  if(is.null(json)){
    return(NULL)
  }
  a <- unlist(jsonlite::fromJSON(json))
  values <-  data.frame(row.names = 1)
  for(roi in rois){
    d <-  data.frame(0,0)
    colnames(d) <- paste0(roi,c(".pre",".post"))
    if (!is.null(a)){
      b <-  a[startsWith(names(a),paste0(roi,"."))]
      d[names(b)] <-  b
    }
    values <- cbind(values,d)
  }
  values
}




##' @title Get a matrix for connectivity between neuron/neuronlist objects
#'
#' @description Get an adjacency matrix for the synaptic connectivity between \code{nat::neuron}/\code{nat::neuronlist} objects. This function does not query a neuPrint server.
#' It uses information on synaptic connectivity stored in a \code{nat::neuron}/\code{nat::neuronlist} object, as read from neuPrint by \code{neuprint_read_neurons}.
#' This can be particularly useful if you have neurons that you have been pruned using \code{nat:prune} family functions, because you just want to know the connectivity associated with
#' this modified skeleton, and not all connectivity associated with a bodyid on neuPrint.
#' @param pre a neuron/neuronlist object. Putative input neurons (rows of returned matrix). If \code{post} is \code{NULL}, then these are also the putative target neurons (columns of returned matrix)
#' @param post a neuron/neuronlist object. Putative target neurons. Defaults to \code{NULL}
#' @param ... methods sent to \code{nat::nlapply}
#' @return a n x n matrix, where the rows are input neurons and the columns are their targets. Names are bodyids.
#' @seealso \code{\link{neuprint_connection_table}}, \code{\link{neuprint_read_neurons}}, \code{\link{neuprint_common_connectivity}}
#' @examples
#' \donttest{
#' neurons = neuprint_read_neurons(c(818983130,1143677310))
#' M = neuprint_skeleton_connectivity_matrix(neurons)
#' stats::heatmap(M)
#' }
#' @export
#' @rdname neuprint_skeleton_connectivity_matrix
neuprint_skeleton_connectivity_matrix <- function (pre, post = NULL, ...) {
  pre = nat::as.neuronlist(pre)
  outs = nat::nlapply(pre, function(x) subset(x$connectors$bodyid,x$connectors$prepost == 0),...)
  if(is.null(post)){
    post = pre
  }else{
    post = nat::as.neuronlist(post)
  }
  ins = nat::nlapply(post, function(x) subset(x$connectors$partner,x$connectors$prepost == 1),...)
  m = matrix(0, nrow = length(pre), ncol = length(post))
  colnames(m) = names(post)
  rownames(m) = names(pre)
  for (skel in names(pre)) {
    for (skel2 in names(post)) {
      a = sum(outs[[skel]]==skel2)
      b = sum(ins[[skel2]]==skel)
      syns = min(a,b)
      m[skel, skel2] <- syns
    }
  }
  m
}
