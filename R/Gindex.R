# return a data.frame with byte addresses of all GRIB messages
# The code only looks at the first sector.
grib_position_index <- function(filename, max_msg=2000) {
  filename <- path.expand(filename)
  if (!file.exists(filename)) stop("File ", filename, "not found.")

  g_index <- .Call("Rgrib_fast_index", filename, as.integer(max_msg))
 
  if (is.null(g_index)) return(NULL)

  names(g_index) <- c("loc", "len", "ed")
  result <- data.frame(g_index)
  class(result) <- c("grib_position_index", class(result))
  attr(result, "filename") <- filename
#  attr(result, "file_pointer") <- file(filename, open="rb")

  result
}

# API for eccodes "index" objects
# keylist must be either a list (or vector) of character strings
# or already a single character string with comm-separated key names
# TODO: you can have all keys as attribute names, so you can always see which values are set.
Gindex <- function(filename, keylist, multi=FALSE) {
  keylist2 <- paste(keylist, collapse=",")
  z <- .Call("Rgrib_index_from_file",
               filename, keylist2, as.integer(multi))
  attr(z, "keylist") <- keylist
  class(z) <- "GRIBindex"
  # this doesn't actually do anything:
  #  for (key in keylist) attr(z, key) <- NULL
  return(z)
}

Gselect <- function(gribindex, keylist=list()) {
  if (!is.list(keylist)) stop("keylist must be a (named) list of key values!")
  .Call("Rgrib_index_select", attr(gribindex, "gribindex_ptr"), keylist)
  for (key in names(keylist)) attr(gribindex, key) <- keylist[[key]]
  return(gribindex)
}



