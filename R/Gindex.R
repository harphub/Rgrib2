# return a data.frame with byte addresses of all GRIB messages
# The co code only looks at the first sector.
position_index <- function(filename, max_msg=2000) {
  filename <- path.expand(filename)
  if (!file.exists(filename)) stop("File ", filename, "not found.")

  g_index <- .Call("Rgrib_fast_index", filename, as.integer(max_msg))
 
  if (is.null(g_index)) return(NULL)

  names(g_index) <- c("loc", "len", "ed")
  result <- data.frame(g_index)
  class(result) <- c("GRIBindex", class(result))
  attr(result, "filename") <- filename
#  attr(result, "file_pointer") <- file(filename, open="rb")

  result
}

Gindex <- function(filename, keylist, multi=FALSE) {
  keylist2 <- paste(keylist, collapse=",")
  z <- .Call("Rgrib_index_from_file",
               filename, keylist2, as.integer(multi))
  attr(z, "keylist") <- keylist
  class(z) <- c("GRIBindex", class(z))
  return(z)
}




