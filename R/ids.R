# private function to convert 1 or more 64 bit ids (e.g. body ids) to JSON
id2json <- function(x, ...) {
  bx=id2bit64(x)
  jsonlite::toJSON(bx, ...)
}

# ... this bit might also be useful
id2bit64 <- function(x) {
  if(is.factor(x)) {
    x=as.character(x)
  }
  if(isTRUE(is.character(x)) || bit64::is.integer64(x)) {
    # do nothing
  } else if(is.integer(x)){
    BIGGESTINT=2147483647L
    if(any(x>BIGGESTINT))
      stop("Some ids cannot be exactly represented! Please use character or bit64!")
  } else if(is.double(x)) {
    # biggest int that can be represented as double 2^(mantissa bits + 1)
    BIGGESTINT=2^53+1
    if(any(x>BIGGESTINT))
      stop("Some ids cannot be exactly represented! Please use character or bit64!")
  } else {
    stop("Unexpected data type for id. Use character, bit64, or numeric!")
  }
  bit64::as.integer64(x)
}
