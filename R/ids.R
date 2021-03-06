# this should return TRUE if we have something that looks bodyid ish
looks_like_bodyid <- function(x) {
  all(is.finite(bit64::as.integer64(x)))
}

# this is more stringent
valid_id <- function(x) {
  converted=tryCatch(id2bit64(x), error=function(e) NA)
  length(converted)>0 && !any(is.na(converted))
}


# private function to convert 1 or more 64 bit ids (e.g. body ids) to JSON
id2json <- function(x, uniqueids=FALSE, ...) {
  bx=id2bit64(x)
  if(any(is.na(bx))) stop("cannot reliably JSON encode NA ids!")
  if(isTRUE(uniqueids)) bx=unique(bx)
  jsonlite::toJSON(bx, ...)
}

# ... this bit might also be useful
id2bit64 <- function(x) {
  if(is.null(x) || length(x)==0) return(bit64::integer64())
  if(is.data.frame(x)) {
    nx=tolower(names(x))
    if('bodyid' %in% nx)
      x=x[[match('bodyid', nx)]]
    else
      stop("If you pass a data.frame, it must contain a `bodyid` column!")
  }
  x=unlist(x, use.names = FALSE)
  if(is.factor(x)) {
    x=as.character(x)
  }
  if(isTRUE(is.character(x))) {
    # bit64::as.integer64("") returns 0 so we need to flag these as NA
    x[nchar(x)==0 | is.na(x)]=NA_character_
  } else if(bit64::is.integer64(x)) {
    # do nothing
  } else if(is.integer(x)){
    BIGGESTINT=2147483647L
    if(any(x>BIGGESTINT))
      stop("Many 64 bit ids cannot be exactly represented as 32 bit ints!\n",
        "Please use character or bit64!")
  } else if(is.double(x)) {
    # biggest int that can be represented as double 2^(mantissa bits + 1)
    BIGGESTFLOAT=2^53+1
    if(any(x>BIGGESTFLOAT, na.rm = TRUE))
      stop("Some 64 bit ids cannot be exactly represented as floating point ",
           "(double) numbers!\nPlease use character or bit64!")
  } else if(is.logical(x) && all(is.na(x))){
    # as a special case let these through because NA, a logical value,
    # is often used instead of NA_real_, NA_character_ etc
  } else {
    stop("Unexpected data type for id. Use character, bit64, or numeric!")
  }
  bx <- bit64::as.integer64(x)
  if(any(bx < 0, na.rm = TRUE))
    stop("Invalid id!")
  # unfortunately if we pass a number >9223372036854775807 then we will get
  # 9223372036854775807. So we must reject ids >= than this.
  if(any(bx>=bit64::as.integer64('9223372036854775807'), na.rm = TRUE))
    stop('I can only cope with ids < 9223372036854775807')
  bx
}

# this is the easiest thing to use in your functions as character vectors
# never get munged (unlike 64 bit ints, which occasionally lose their class)
id2char <- function(x) {
  as.character(id2bit64(x))
}
