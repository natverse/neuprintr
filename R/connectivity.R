#' @title Get a matrix of connectivities between bodies
#'
#' @description  Get an adjacency matrix for the synaptic connectivity within a
#'   set of specified bodies
#' @param inputids,outputids identifiers for input and output bodies (use as an
#'   alternative to \code{bodyids})
#' @param sparse Whether to return a sparse adjacency matrix (of class
#'   \code{\link[=CsparseMatrix-class]{CsparseMatrix}}). Default \code{FALSE}.
#' @param cache the query to neuPrint server, so that it does not need to be
#'   repeated. Of course you can save the results, but this may be helpful e.g.
#'   inside a wrapper function that postprocesses the results like
#'   \code{hemibrainr::grouped_adjacency_matrix}.
#' @inheritParams neuprint_read_neurons
#' @return a n x n matrix, where the rows are input neurons and the columns are
#'   their targets. Only neurons supplied as the argument `bodyids` are
#'   considered.
#' @seealso \code{\link{neuprint_fetch_custom}},
#'   \code{\link{neuprint_simple_connectivity}},
#'   \code{\link{neuprint_common_connectivity}},
#'   \code{\link[=CsparseMatrix-class]{CsparseMatrix}})
#' @export
#' @rdname neuprint_get_adjacency_matrix
#' @examples
#' \donttest{
#' # these will mostly be axo-axonic connections between DA2 PNs
#' neuprint_get_adjacency_matrix('DA2 lPN')
#'
#' # rectangular matrix with different in/out neurons
#' neuprint_get_adjacency_matrix(inputids='DA2 lPN', outputids='DL4 adPN')
#' }
#' \donttest{
#' # Note the use of cache=T, which will avoid a subsequent query to the
#' # neuPrint server if the same information is requested
#' pnkc=neuprint_get_adjacency_matrix(inputids='name:mPN', outputids='/KC.*',
#'   cache=TRUE)
#' hist(colSums(pnkc), xlab = 'PN inputs / KC', br=100)
#' sum(rowSums(pnkc)>0)
#' }
#' \dontrun{
#' # sparse adjacency matrix
#' pnkcs=neuprint_get_adjacency_matrix(inputids='name:mPN',
#'   outputids='/KC.*', sparse=TRUE, cache=TRUE)
#' library(Matrix)
#' # PN-KC connectivity is itself sparse, so < 2% of entries are non zero
#' nnzero(pnkcs)/length(pnkcs)
#' # while memory requirements are ~ 5%
#' as.numeric(object.size(pnkcs)/object.size(pnkc))
#' }
#' @importFrom Matrix sparseMatrix
neuprint_get_adjacency_matrix <- function(bodyids=NULL, inputids=NULL,
                                          outputids=NULL, dataset = NULL,
                                          all_segments = FALSE, conn = NULL,
                                          sparse=FALSE, cache=FALSE, ...){
  conn=neuprint_login(conn)
  if(is.null(bodyids)) {
    if(is.null(inputids) || is.null(outputids))
      stop("You must either specify bodyids OR (inputids AND outputids)!")
    inputids=neuprint_ids(inputids, conn=conn, dataset = dataset, cache=cache)
    outputids=neuprint_ids(outputids, conn=conn, dataset = dataset, cache=cache)
  } else {
    if(!is.null(inputids) || !is.null(outputids))
      stop("You must either specify bodyids OR (inputids AND outputids)!")
    inputids <- neuprint_ids(bodyids, conn=conn, dataset = dataset, cache=cache)
    outputids <- inputids
  }
  outputids=id2bit64(outputids)
  inputids=id2bit64(inputids)
  all_segments.json = ifelse(all_segments,"Segment","Neuron")
  namefield=neuprint_name_field(conn=conn, dataset=dataset)
  cypher = sprintf(
    paste(
      "WITH %s AS input, %s AS output MATCH (n:`%s`)-[c:ConnectsTo]->(m)",
      "WHERE n.bodyId IN input AND m.bodyId IN output",
      "RETURN n.bodyId AS upstream, m.bodyId AS downstream, c.weight AS weight"
    ),
    id2json(inputids),
    id2json(outputids),
    all_segments.json,
    namefield,
    namefield
  )
  nc = neuprint_fetch_custom(cypher=cypher, conn = conn, dataset = dataset,
                             cache=cache, ...)
  df = neuprint_list2df(nc, return_empty_df = TRUE)
  df$weight=as.integer(df$weight)
  df$upstream=id2bit64(df$upstream)
  df$downstream=id2bit64(df$downstream)
  sm = sparseMatrix(
    i = match(df$upstream, inputids),
    j = match(df$downstream, outputids),
    x = df$weight,
    dims = c(length(inputids), length(outputids)),
    dimnames = list(inputids, outputids)
  )
  if(isTRUE(sparse)) sm else as.matrix(sm)
}

#' @title Get the upstream and downstream connectivity of a neuron
#'
#' @description Get the upstream and downstream connectivity of a body,
#'   restricted to within an ROI if specified
#' @inheritParams neuprint_read_neurons
#' @inheritParams neuprint_find_neurons
#' @param prepost \code{PRE}: look for partners presynaptic (i.e upstream
#'   inputs) or \code{POST}: postsynaptic (downstream outputs) to the given
#'   \code{bodyids}
#' @param by.roi logical, whether or not to break neurons' connectivity down by
#'   region of interest (ROI)
#' @param details When \code{TRUE} returns adds a name and type column for
#'   partners.
#' @param threshold Only return partners >= to an integer value. Default of 1
#'   returns all partners. This threshold will be applied to the ROI weight when
#'   the \code{roi} argument is specified, otherwise to the whole neuron.
#' @param superLevel When \code{by.roi=TRUE}, should we look at low-level ROIs
#'   (\code{superLevel=FALSE}) or only super-level ROIs
#'   (\code{superLevel=TRUE}). A super-level ROIs can contain multiple
#'   lower-level ROIs. If set to `NULL`, both are returned.
#' @param chunk A logical specifying whether to split the query into multiple
#'   chunks or an integer specifying the size of those chunks (which defaults to
#'   20 when \code{chunk=TRUE}).
#' @param progress default FALSE. If TRUE, the API is called separately for each
#'   neuron and you can assess its progress, if an error is thrown by any one
#'   \code{bodyid}, that \code{bodyid} is ignored
#' @return a data frame with one row for each unique combination of query
#'   neuron, partner neuron and (where specified) ROI. \itemize{
#'
#'   \item bodyid query neuron identifier
#'
#'   \item partner neuron identifier
#'
#'   \item prepost 0 for downstream/output partners; 1 for upstream/input
#'   partners
#'
#'   \item weight total number of connections between the query and partner
#'   neuron.
#'
#'   }. When \code{by.roi=TRUE}
#'
#'   \itemize{
#'
#'   \item roi the name of the ROI
#'
#'   \item ROIweight the number of connections within that ROI
#'
#'   }
#'
#'   Note that when \code{by.roi=TRUE} there may be multiple rows for each
#'   unique pair of neurons \bold{and} connections may appear in more than one
#'   row when some ROIs are contained within larger ROIs. For example the
#'   \code{CA(R)} is contained within the \code{MB(R)} ROI. Therefore you cannot
#'   sum the \code{ROIweight} to match the total connection strength (which is
#'   available as the \code{weight} column).
#'
#' @seealso \code{\link{neuprint_fetch_custom}},
#'   \code{\link{neuprint_simple_connectivity}},
#'   \code{\link{neuprint_common_connectivity}}, \code{\link{neuprint_ROIs}}
#' @export
#' @rdname neuprint_connection_table
#' @examples
#' \donttest{
#' ## Unitary connection strengths between two chosen neurons,
#' ## and their downstream targets
#' c1 = neuprint_connection_table(c(818983130, 1796818119), prepost = "POST")
#' head(c1)
#'
#' ## The same connections broken down by ROI
#' c2 = neuprint_connection_table(c(818983130, 1796818119), prepost = "POST",
#'                                by.roi = TRUE)
#' head(c2)
#'
#' ## The same connections broken down by super-level ROIs only
#' c3 = neuprint_connection_table(c(818983130, 1796818119), prepost = "POST",
#'                                by.roi = TRUE, superLevel = TRUE)
#' nrow(c3)
#' nrow(c2)
#'
#' ## Find only the connections within a specific ROI
#' c4 = neuprint_connection_table(c(818983130, 1796818119), prepost = "POST",
#'                                by.roi = TRUE, roi = "LH(R)")
#'
#' }
#' @importFrom checkmate assert_integer
neuprint_connection_table <- function(bodyids,
                                      prepost = c("PRE","POST"),
                                      roi = NULL,
                                      by.roi = FALSE,
                                      threshold=1L,
                                      details=FALSE,
                                      superLevel = FALSE,
                                      progress = FALSE,
                                      dataset = NULL,
                                      chunk=TRUE,
                                      all_segments = FALSE,
                                      conn = NULL,
                                      ...){
  prepost <- match.arg(prepost)
  conn<-neuprint_login(conn)
  bodyids <- neuprint_ids(bodyids, dataset = dataset, conn = conn)

  threshold=assert_integer(as.integer(round(threshold)), lower = 1, len = 1)

  nP <- length(bodyids)
  if(is.numeric(chunk)) {
    chunksize=chunk
  } else {
    # make smaller chunks when progress=T and there aren't so many bodyids
    if (chunk ==TRUE)
      if(isTRUE(progress))
        chunksize=min(20L, ceiling(nP/10))
      else
        chunksize=20L
      else
        chunksize=Inf
  }

  if(nP>chunksize) {
    nchunks=ceiling(nP/chunksize)
    chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nP)]
    bodyids <- split(bodyids, chunks)
    # if we got here and progess is unset then set it
    if(is.null(progress) || is.na(progress)) progress=TRUE
    MYPLY <- if(isTRUE(progress)) pbapply::pblapply else lapply
    d  = dplyr::bind_rows(MYPLY(bodyids, function(bi) tryCatch(neuprint_connection_table(
      bodyids = bi,
      prepost = prepost,
      roi = roi,
      by.roi = by.roi,
      threshold = threshold,
      details=details,
      progress = FALSE,
      dataset = dataset, conn = conn, ...),
      error = function(e) {warning(e); NULL})))
    d <-  d[order(d$weight,decreasing=TRUE),]
    rownames(d) <- NULL
    return(d)
  }


  all_segments.json <- ifelse(all_segments,"Segment","Neuron")

  if(!is.null(roi)){
    roicheck <- neuprint_check_roi(rois=roi, dataset = dataset, conn = conn, superLevel = superLevel , ...)
  }

  WITH=sprintf("WITH %s AS bodyIds UNWIND bodyIds AS bodyId",id2json(bodyids))

  MATCH=sprintf("MATCH (a:`%s`)-[c:ConnectsTo]->(b:`%s`)",
                all_segments.json, all_segments.json)

  WHERE=sprintf("WHERE %s.bodyId=bodyId %s %s",
                ifelse(prepost=="POST","a","b"),
                ifelse(threshold>1, paste("AND c.weight >= ", threshold), ""),
                ifelse(!is.null(roi)|by.roi,
                       "UNWIND keys(apoc.convert.fromJsonMap(c.roiInfo)) AS k",""))

  extrafields <- if(isTRUE(details)) {
    ab=ifelse(prepost=="PRE","a","b")
    sprintf(", %s.type AS type, %s.instance AS name", ab, ab)
  } else ""
  RETURN=sprintf("RETURN a.bodyId AS %s, b.bodyId AS %s, c.weight AS weight %s %s",
                 ifelse(prepost=="POST","bodyid","partner"),
                 ifelse(prepost=="POST","partner","bodyid"),
                 extrafields,
                 ifelse(!is.null(roi)|by.roi,", k AS roi, apoc.convert.fromJsonMap(c.roiInfo)[k].post AS ROIweight","")

  )
  cypher <-paste(WITH, MATCH, WHERE, RETURN)

  nc <-neuprint_fetch_custom(cypher=cypher, conn = conn, dataset = dataset, ...)
  ## Filter out the rare cases where PSDs and tbars are in different ROIs (hence post is null)
  if(!is.null(roi)|by.roi){
    roicol=match("roi", unlist(nc$columns))
    nc$data <- nc$data[sapply(nc$data,function(x) !is.null(x[[roicol]]))]
  }
  d <- neuprint_list2df(nc, return_empty_df = TRUE)
  d$weight <- as.integer(d$weight)
  if(nrow(d)==0) {
    d$prepost=integer(length = 0L)
  } else {
    d$prepost <-  ifelse(prepost=="PRE",0,1)
  }
  if(!is.null(roi)){
    d <- d[d$roi%in%roi,]
  }
  if(by.roi && is.null(roi)){
    rois <- neuprint_ROIs(superLevel = superLevel)
    d <- d[d$roi%in%rois,]
  }
  d <-  d[order(d$weight,decreasing=TRUE),]
  rownames(d) <- NULL
  firstcols=sort(c(colnames(d)[1:3], 'prepost'))
  if(details)
    firstcols=c(firstcols, 'name', 'type')
  othercols=setdiff(colnames(d), firstcols)
  d=d[,c(firstcols, sort(othercols))]

  if(!is.null(roi) && threshold>1)
    d=d[d$ROIweight>=threshold,]

  d
}

#' @title Get the common synaptic partners for a set of neurons
#'
#' @description  Get the neurons that are shared synaptic partners for a set of
#'   given bodyids, either upstream or downstream.
#' @param bodyids the cypher by which to make your search
#' @param statuses if not NULL, only bodies with the given status are
#'   considered. Statuses include: Unimportant,0.5assign,Leaves,Prelim Roughly
#'   Traced, Anchor, Orphan.
#' @param prepost whether to look for partners presynaptic or postsynaptic to
#'   the given bodyids. So when \code{prepost="PRE"} you will return inputs
#'   (upstream partners) of your starting neurons.
#' @param all_segments if TRUE, all bodies are considered, if FALSE, only
#'   'Neurons', i.e. bodies with a status roughly traced status.
#' @param ... methods passed to \code{neuprint_login}
#' @inheritParams neuprint_fetch_custom
#' @return a n x m matrix where n correspond to the neurons that all connect to
#'   m bodyids
#' @seealso \code{\link{neuprint_simple_connectivity}},
#'   \code{\link{neuprint_get_adjacency_matrix}}
#' @export
#' @rdname neuprint_common_connectivity
#' @examples
#' \donttest{
#' da2s=neuprint_search('.*DA2.*')
#' da2conn = neuprint_common_connectivity(da2s$bodyid[1:2], prepost='PRE')
#' plot(t(da2conn))
#' head(cbind(t(da2conn), sum=colSums(da2conn)))
#' }
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
  find_inputs = ifelse(prepost=="POST", "false","true")
  all_segments = ifelse(all_segments,"true","false")

  conn=neuprint_login(conn)
  dataset = check_dataset(dataset, conn=conn)
  bodyids <- neuprint_ids(bodyids, dataset = dataset, conn = conn)

  Payload = noquote(sprintf('{"dataset":"%s","neuron_ids":%s,"statuses":%s,"find_inputs":%s,"all_segments":%s}',
                            dataset,
                            id2json(bodyids),
                            ifelse(is.null(statuses),jsonlite::toJSON(list()),jsonlite::toJSON(statuses)),
                            find_inputs,
                            all_segments))
  class(Payload) = "json"
  com.conn = neuprint_fetch(path = 'api/npexplorer/commonconnectivity', body = Payload, conn = conn, ...)
  partnerType <- ifelse(prepost=="POST","output","input")
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
#' @param ... methods passed to \code{neuprint_login}
#' @inheritParams neuprint_fetch_custom
#' @return a n x m matrix where n correspond to the neurons that connect to m
#'   bodyids
#' @examples
#' \donttest{
#' da2s=neuprint_search(".*DA2.*")
#' neuprint_common_connectivity(da2s$bodyid)
#' }
#' @seealso \code{\link{neuprint_common_connectivity}},
#'   \code{\link{neuprint_get_adjacency_matrix}}
#' @export
#' @examples
#' inputs <- neuprint_simple_connectivity(5901222731, prepost='PRE')
#' head(inputs)
#' # top inputs
#' head(sort(table(inputs$type), decreasing = TRUE))
#' outputs <- neuprint_simple_connectivity(5901222731, prepost='POST')
#' head(outputs)
neuprint_simple_connectivity <- function(bodyids,
                                         prepost = c("PRE","POST"),
                                         dataset = NULL,
                                         conn = NULL,
                                         ...){
  prepost = match.arg(prepost)
  conn=neuprint_login(conn)
  dataset = check_dataset(dataset, conn=conn)
  bodyids <- neuprint_ids(bodyids, dataset = dataset, conn = conn)
  if(length(bodyids)>10) {
    m  = Reduce(function(x, y, ...)
      dplyr::full_join(x, y, by = c(
        "name", ifelse(prepost=="PRE","input","output"), "type"
      )),
      (pbapply::pblapply(bodyids, function(bi)
        tryCatch(
          neuprint_simple_connectivity(
            bodyids = bi,
            prepost = prepost,
            dataset = dataset,
            conn = conn,
            ...
          ),
          error = function(e) {warning(e); NULL}
        ))))
    # FIXME need to convert NA weights to 0
    return(m)
  }

  find_inputs = ifelse(prepost=="PRE", "true", "false")
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
#' @param all_segments if TRUE, all bodies are considered, if FALSE, only
#'   'Neurons', i.e. bodies with a status roughly traced status.
#' @param roi Limit the search to connections happening within a certain ROI or
#'   set of ROIs (NULL by default)
#' @param by.roi Return the results by ROI. Default to FALSE
#' @param exclude.loops Whether or not to exclude loops
#' (paths containing the same node several times). Defaults to TRUE
#' @param chunk A logical specifying whether to split the query into multiple
#'   chunks or an integer specifying the size of those chunks (which defaults
#'   to 5 when \code{chunk=TRUE}).
#' @param progress if TRUE, a progress bar will be shown. This may slow the data
#'   fetching process for smaller queries. The default of
#'   \code{progress=NULL} will only show a progress bar if the query will be
#'   split into multiple chunks based on the \code{chunk} argument.
#' @param ... methods passed to \code{neuprint_login}
#' @inheritParams neuprint_fetch_custom
#' @seealso \code{\link{neuprint_get_shortest_paths}},
#'   \code{\link{neuprint_common_connectivity}},
#'   \code{\link{neuprint_get_adjacency_matrix}}
#' @export
#' @examples
#' \donttest{
#' neuprint_get_paths(c(1128092885,481121605),5813041365, n=c(1,2), weightT=20)
#'
#' neuprint_get_paths(c(1128092885,481121605),5813041365, n=c(1,2),
#'   weightT=20,by.roi=TRUE)
#'
#' neuprint_get_paths(c(1128092885,481121605),5813041365,
#'   n=c(1,2), weightT=20,roi=c("FB","LAL(-GA)(R)"))
#' }
neuprint_get_paths <- function(body_pre, body_post, n, weightT=5, roi=NULL, by.roi=FALSE,exclude.loops=TRUE,
                               chunk=TRUE,progress=FALSE,dataset = NULL, conn = NULL, all_segments=FALSE, ...){

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
    roiQ <- paste("(" ,paste0("apoc.convert.fromJsonMap(x.roiInfo).`",roi,"`.post >=",weightT,collapse=" OR "),") AND ")
  }

  body_pre <- neuprint_ids(body_pre, dataset = dataset, conn = conn)
  body_post <- neuprint_ids(body_post, dataset = dataset, conn = conn)

  nP <- length(body_pre)
  if(is.numeric(chunk)) {
    chunksize=chunk
  } else {
    # make smaller chunks when progress=T and there aren't so many bodyids
    if (chunk ==TRUE)
      if(isTRUE(progress))
        chunksize=min(5L, ceiling(nP/10))
      else
        chunksize=5L
    else
      chunksize=Inf
  }

  if(nP>chunksize) {
    nchunks=ceiling(nP/chunksize)
    chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nP)]
    body_pre <- split(body_pre, chunks)
    # if we got here and progess is unset then set it
    if(is.null(progress) || is.na(progress)) progress=TRUE
    MYPLY <- if(isTRUE(progress)) pbapply::pblapply else lapply
    d  = dplyr::bind_rows(MYPLY(body_pre, function(pre) tryCatch(neuprint_get_paths(
      body_pre = pre,
      body_post = body_post,
      n=n,
      weightT = weightT,
      roi = roi,
      by.roi = by.roi,
      exclude.loops = exclude.loops,
      progress = FALSE,
      chunk = FALSE,
      dataset = dataset,
      conn = conn,
      all_segments=all_segments,
      ...),
      error = function(e) {warning(e); NULL})))
    return(d)
  }

  all_segments.json <-  ifelse(all_segments,"Segment","Neuron")
  cypher <-  sprintf(paste("MATCH p = (src:`%s`)-[ConnectsTo*%s..%s]->(`%s`)",
                           "WHERE src.bodyId IN %s AND %s last(nodes(p)).bodyId IN %s AND",
                           "ALL(x in relationships(p) WHERE %s x.weight>=%s)",
                           "RETURN length(p) AS `length(path)`,[n in nodes(p) | [n.bodyId, n.instance, n.type]] AS path,[x in relationships(p) | x.weight] AS weights %s"
  ),
  all_segments.json,
  n[1]-1,
  n[2],
  all_segments.json,
  id2json(body_pre),
  ifelse(exclude.loops,"(NOT apoc.coll.containsDuplicates(nodes(p))) AND",""),
  id2json(body_post),
  ifelse(is.null(roi),"",roiQ),
  weightT,
  ifelse(is.null(roi) & by.roi==FALSE,"",paste0(", [x in relationships(p) | x.roiInfo] AS roiInfo"))
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

  if (!is.null(roi) | by.roi){
    roiTable <- dplyr::bind_rows(lapply(nc$data, function(d){
      l <- d[[1]]
      if (by.roi == TRUE){roi="All"}
      dplyr::bind_rows(lapply(d[[4]],function(dT){extract_connectivity_df(roi,dT,"post")}))
    }))
    roiTable[is.na(roiTable)] <- 0
    connTable <- dplyr::bind_cols(connTable,roiTable)
  }

  as.data.frame(connTable)
}

#' @title Get a list of the shortest paths between two neurons
#'
#' @description  Get all of the shortest paths in the database that connect the
#'   query neurons with at least weightT synapses at each step
#' @param body_pre the bodyid of the neuron at the start of the path
#' @param body_post the bodyid of the neuron at the end of the path
#' @param weightT weight threshold
#' @param roi Limit the search to connections happening within a certain ROI or
#'   set of ROIs (NULL by default)
#' @param by.roi Return the results by ROI. Default to FALSE
#' @param all_segments if TRUE, all bodies are considered, if FALSE, only
#'   'Neurons', i.e. bodies with a status roughly traced status.
#' @param chunk A logical specifying whether to split the query into multiple
#'   chunks or an integer specifying the size of those chunks (which defaults
#'   to 5 when \code{chunk=TRUE}).
#' @param progress if TRUE, a progress bar will be shown. This may slow the data
#'   fetching process for smaller queries. The default of
#'   \code{progress=NULL} will only show a progress bar if the query will be
#'   split into multiple chunks based on the \code{chunk} argument.
#' @param ... methods passed to \code{neuprint_login}
#' @inheritParams neuprint_fetch_custom
#' @seealso \code{\link{neuprint_get_paths}},
#'   \code{\link{neuprint_common_connectivity}},
#'   \code{\link{neuprint_get_adjacency_matrix}}
#' @export
#' @examples
#' \donttest{
#' neuprint_get_shortest_paths(c(1128092885,481121605),5813041365,weightT=20)
#' }
neuprint_get_shortest_paths <- function(body_pre,body_post,weightT=5,roi=NULL,by.roi=FALSE,chunk=TRUE,progress=FALSE,dataset = NULL, conn = NULL,all_segments=FALSE, ...){

  conn <- neuprint_login(conn)

  body_pre <- neuprint_ids(body_pre, dataset = dataset, conn = conn)
  body_post <- neuprint_ids(body_post, dataset = dataset, conn = conn)

  nP <- length(body_pre)
  if(is.numeric(chunk)) {
    chunksize=chunk
  } else {
    # make smaller chunks when progress=T and there aren't so many bodyids
    if (chunk ==TRUE)
      if(isTRUE(progress))
        chunksize=min(5L, ceiling(nP/10))
      else
        chunksize=5L
    else
      chunksize=Inf
  }

  if(nP>chunksize) {
    nchunks=ceiling(nP/chunksize)
    chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nP)]
    body_pre <- split(body_pre, chunks)
    # if we got here and progess is unset then set it
    if(is.null(progress) || is.na(progress)) progress=TRUE
    MYPLY <- if(isTRUE(progress)) pbapply::pblapply else lapply
    d  = dplyr::bind_rows(MYPLY(body_pre, function(pre) tryCatch(neuprint_get_shortest_paths(
      body_pre = pre,
      body_post = body_post,
      weightT = weightT,
      roi = roi,
      by.roi = by.roi,
      progress = FALSE,
      chunk = FALSE,
      dataset = dataset,
      conn = conn,
      all_segments=all_segments,
      ...),
      error = function(e) {warning(e); NULL})))
    return(as.data.frame(d))
  }

  all_segments.json <-  ifelse(all_segments,"Segment","Neuron")

  if(!is.null(roi)){
    roicheck = neuprint_check_roi(rois=roi, dataset = dataset, conn = conn, ...)
    roiQ <- paste("(" ,paste0("apoc.convert.fromJsonMap(x.roiInfo).`",roi,"`.post >=",weightT,collapse=" OR "),") AND ")
  }
  cypher <-  sprintf(paste("MATCH p = allShortestPaths((src: `%s`)-[c: ConnectsTo*]->(dest:`%s`))",
                           "WHERE src.bodyId IN %s AND dest.bodyId IN %s AND src.bodyId <> dest.bodyId AND",
                           "ALL (x in relationships(p) WHERE %s x.weight >= %s)",
                           "RETURN length(p) AS `length(path)`,[n in nodes(p) | [n.bodyId, n.instance, n.type]] AS path,[x in relationships(p) | x.weight] AS weights %s"
  ),
  all_segments.json,
  all_segments.json,
  id2json(body_pre),
  id2json(body_post),
  ifelse(is.null(roi),"",roiQ),
  weightT,
  ifelse(is.null(roi) & by.roi==FALSE,"",paste0(", [x in relationships(p) | x.roiInfo] AS roiInfo"))
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

  if (!is.null(roi) | by.roi){
    roiTable <- dplyr::bind_rows(lapply(nc$data, function(d){
      l <- d[[1]]
      if (by.roi == TRUE){roi="All"}
      dplyr::bind_rows(lapply(d[[4]],function(dT){extract_connectivity_df(roi,dT,"post")}))
    }))
    roiTable[is.na(roiTable)] <- 0
    connTable <- dplyr::bind_cols(connTable,roiTable)
  }

  connTable
}

# hidden, caution, does not deal with left/right neuropils
extract_connectivity_df <- function(rois, json, postFix  = c("pre", "post")){
  if(is.null(json)){
    return(NULL)
  }

  a <- unlist(jsonlite::fromJSON(json))
  if(isTRUE(tolower(rois)=="all")) {
    rois <- gsub("\\..*","",names(a))
  }
  rois <- unique(rois) #this takes care if both the input and output ROIs are same..
  roicols <- c(t(outer(rois,postFix, paste, sep=".")))
  values <- structure(rep(0L, length(roicols)), .Names=roicols)
  for(roi in rois){
    thisroicols <- paste0(roi,".",postFix)
    if (!is.null(a)){
      b <-  a[names(a) %in% thisroicols]
      values[names(b)] <-  b
    }
  }
  values <- tibble::as_tibble(as.list(values))
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
  outs = nat::nlapply(pre, function(x) subset(x$connectors$partner,x$connectors$prepost == 0),...)
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
