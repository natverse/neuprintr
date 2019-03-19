# hidden
neuprint_fetch <- function(path, body = NULL, conn = NULL, parse.json = TRUE,
                           include_headers = TRUE, simplifyVector = FALSE, ...){
  path = gsub("\\/$|^\\/","",path)
  conn = neuprint_login(conn)
  req <-
    if (is.null(body)) {
      httr::GET(url = file.path(conn$server, path, fsep = "/"),
                config = conn$config,  ...)
    }else {
      httr::POST(url = file.path(conn$server, path, fsep = "/"),
           body = body, config = conn$config, ...)
    }
  httr::stop_for_status(req)
  if (parse.json) {
    parsed = neuprint_parse_json(req, simplifyVector = simplifyVector)
    if (length(parsed) == 2 && isTRUE(names(parsed)[2] =="error")) {
      stop("neuprint error: ", parsed$error)
    }
    if (include_headers) {
      fields_to_include = c("url", "headers")
      attributes(parsed) = c(attributes(parsed), req[fields_to_include])
    }
    parsed
  }
  else req
}

# hidden
neuprint_parse_json <- function (req, simplifyVector = FALSE, ...) {
  text <- httr::content(req, as = "text", encoding = "UTF-8")
  if (identical(text, ""))
    stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = simplifyVector, ...)
}
