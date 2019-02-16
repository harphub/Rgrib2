#-------------------------------------------#
# Part of R-package Rgrib2                  #
# Copyright (c) 2003-2019 Alex Deckmyn      #
#   Royal Meteorological Institute, Belgium #
# alex.deckmyn@meteo.be                     #
# Released under GPL-3 license              #
#-------------------------------------------#

Ghandle <- function(x, message=1, multi=FALSE){
### create a GRIBhandle from a file and message number
  ### TODO: if the GRIBlist has bit location/length -> read msg directly
  if (is.raw(x)) {
    if (message > 1) {
      gloc <- grib_raw_find(x)
      if (message > dim(gloc)[1]) stop("Only",dim(gloc)[1],"GRIB records available.")
      x <- x[gloc$first[message]:gloc$last[message]]
    }
    gribhandle <-  .Call("Rgrib_handle_new_msg",
                         msg=x, msglen=as.integer(length(x)))
  } else {
    if (inherits(x, "GRIBlist")) {
      filename <- attributes(x)$filename
    } else if (is.character(x)) {
      filename <- path.expand(x)
    } else {
      stop("Not a valid file name or GRIB handle.")
    }

    if (!file.exists(filename)) stop(paste("File",filename,"not found."))
    gribhandle <- .Call("Rgrib_handle_new_file",
                        filename, as.integer(message), multi)
  }
  if (!is.null(gribhandle)) class(gribhandle) <- c(class(gribhandle),"GRIBhandle")
  gribhandle
}


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




