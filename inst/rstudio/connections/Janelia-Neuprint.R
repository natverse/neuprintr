library(neuprintr)

connspec <- neuprintr::neuprint_connection(
  server="${1:Server=https$colon$//neuprint.janelia.org}",
  token = "${2:Neuprint Token=}",
  dataset = "${3:Dataset=hemibrain$colon$v1.2.1}"
)
if(!nzchar(connspec$token)) {
  connspec$token=neuprintr:::getenvoroption('token')[[1]]
  if(is.null(connspec$token))
    stop("Sorry you must either specify your neuprint token directly\n",
         "or set the NEUPRINT_TOKEN environment variable!")
}

conn <- neuprintr::neuprint_login(connspec)
