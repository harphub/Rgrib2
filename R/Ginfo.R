###
### The main routine for getting parameters from a GRIB file or record
###
Ginfo <- function(x, ...){
  UseMethod("Ginfo")
}

Ginfo.GRIBhandle <- function(x, IntPar=c(), DblPar=c(), StrPar=c(), ...)  {
  result <- .Call("Rgrib_handle_info",
                  attr(x,"gribhandle_ptr"), StrPar, IntPar, DblPar)
  if (length(result)>0) {
    result <- data.frame(result, stringsAsFactors=FALSE)
    names(result) <- c(StrPar, DblPar, IntPar)
  }
  result
}

Ginfo.GRIBlist <- function(x, IntPar=c(), DblPar=c(), StrPar=c(),
                           rList=NULL, multi=FALSE, ...) {
  filename <- attr(x, "filename")
  Ginfo.character(filename, IntPar, DblPar, StrPar, rList, multi)
}

Ginfo.GRIBindex <- function(x, IntPar=c(), DblPar=c(), StrPar=c()) {
  .Call("Rgrib_index_info", x, IntPar, DblPar, StrPar)
}

Ginfo.character <- function(x, IntPar=c(), DblPar=c(), StrPar=c(),
                            rList=NULL, multi=FALSE, ...){
  if (is.null(rList) ) {
    nmessages <- .C("Rgrib_count_messages",
                    filename=x, nmessages=integer(1),
                 multi=as.integer(multi))$nmessages
    rList <- 1:nmessages
  }
  filename <- path.expand(x)
  if (!file.exists(filename)) stop(paste("File",filename,"not found."))
  result <- .Call("Rgrib_parse_file",
                  filename, IntPar, DblPar, StrPar,
                  as.integer(rList), multi=multi)
  result <- cbind(rList, data.frame(result, stringsAsFactors=FALSE))
  names(result) <- c("position", StrPar, DblPar, IntPar)
  result
}


