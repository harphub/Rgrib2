Ghandle <- function(x, message=1, multi=FALSE){
### create a GRIBhandle from a file and message number (or a key list)
  ### TODO: if the GRIBlist has bit location/length -> read msg directly
  ### raw GRIB data: what if it's multi-message???
  if (is.list(message)) {
    if (inherits(x, "GRIBlist")) x <- attr(x, "filename")
    if (!is.character(x)) {
      stop("Key search only for filename and a list of keys")
    }
    gribhandle <- .Call("Rgrib_fast_find", filename=x, keys=message, multi=multi)
  } else if (is.raw(x)) {
    if (message > 1) {
      gloc <- grib_raw_find(x)
      if (message > dim(gloc)[1]) stop("Only",dim(gloc)[1],"GRIB records available.")
      x <- x[gloc$first[message]:gloc$last[message]]
    }
    gribhandle <-  .Call("Rgrib_handle_new_msg",
                         msg=x, msglen=as.integer(length(x)))
  } else if (inherits(x, "grib_index")) {
    if (message > dim(x)[1]) stop("Only ", dim(x)[1], " messages in file.")
    gfile <- file(attr(x, "filename"), open="rb") # file(attr(x, "filename"), open="rb")
    on.exit(try(close(gfile), silent=TRUE))
    seek(gfile, x$loc[message], rw="read")
    msg <- readBin(gfile, "raw", x$len[message])
    gribhandle <-  .Call("Rgrib_handle_new_msg",
                         msg=msg, msglen=as.integer(x$len[message]))
  } else {
    if (inherits(x, "GRIBlist")) {
      filename <- attr(x, "filename")
      if ("msg_loc" %in% names(x)) {
        msg_loc <- x$msg_loc[message]
      # we are skipping to right place in the file
      # it may be a sub_field, but that is not yet supported
        sub_message <- 1
      } else {
        # no bit location given: just go to start of file and count...
        msg_loc <- 0
        sub_message <- message
      }
    } else if (is.character(x)) {
      filename <- path.expand(x)
      msg_loc <- 0
      sub_message <- message
    } else {
      stop("Not a valid file name or GRIB handle.")
    }

    if (!file.exists(filename)) stop(paste("File ", filename, " not found."))
    gribhandle <- .Call("Rgrib_handle_new_file2",
                        filename, as.double(msg_loc), as.integer(sub_message), multi)
  }
  if (!is.null(gribhandle)) class(gribhandle) <- c(class(gribhandle),"GRIBhandle")
  gribhandle
}

#Ghandle.grib_index <- function(x, message=1, multi=FALSE){
#  gfile <- attr(x, "filename")
#  on.exit(close(gfile)
#  seek(gfile, x$pos[i], rw="read")
#
#  msg <- readBin(gfile, "raw", x$len[i])
#  Ghandle.raw(msg, 1)
#}
### Admin

GhandleFree <- function(gribhandle){
  if (!inherits(gribhandle,"GRIBhandle")) stop("Not a GRIBhandle.")
  invisible(.Call("Rgrib_clear_handle",attr(gribhandle,"gribhandle_ptr")))
}

close.GRIBhandle <- GhandleFree

GhandleFreeAll <- function(){
  .Call("Rgrib_clear_all_handles")
  invisible(NULL)
}

GhandleCount <- function(){
  .Call("Rgrib_count_handles")
}

GhandleList <- function(){
  .Call("Rgrib_list_handles")
}

print.GRIBhandle <- function(x,...){
  cat("GRIBhandle (ID=",as.integer(x),")\n")
  if (!is.null(attr(x,"filename"))) {
    cat("from file",attr(x,"filename"),"\n")
    cat("message number",attr(x,"message"),"\n")
  }
  if (!is.null(attr(x,"sample"))) {
    cat("from sample",attr(x,"sample"),"\n")
  }
}




