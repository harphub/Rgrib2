"print.GRIBlist" <-
function(x,...){
  cat("File", attr(x, "filename"), ":", "\n")
  cat("containing ", attr(x, "nfields"), "fields.\n")
}
####################################
### find a specific message
### this is the "easy but slow" implementation: first get a complete table and then search in R
Glocate <- function(filename,IntPar=list(),DblPar=list(),StrPar=list(),...){
  gribtab <- Ginfo(filename,IntPar=names(IntPar),DblPar=names(DblPar),StrPar=names(StrPar),
                   rList=NULL,...)
  allpar <- c(IntPar,DblPar,StrPar)
  subs <- paste(names(allpar),"==\"",allpar,"\"",sep="",collapse=" & ")
  result <- subset(gribtab,subset=eval(parse(text=subs)),
                   select="position")$position   # use quotes to satisfy "check"
  if (length(result)==0) NA
  else result
}

### a more basic version for parameter (number or shortName), 
### leave parameter list "open" (,...) for later additions
Gfind <- function(griblist, shortName="t", level=NULL, levelType="P",
                  all=FALSE, ...){
  if (is.character(griblist)) {
    filename <- griblist
    if (!file.exists(filename)) stop(paste("File",filename,"not found."))
    griblist <- Gopen(filename)
  }

  if (!is.null(level)){
    levelType <- switch(levelType,
              "P"=100,
              "H"=105,
              "S"=109,
              levelType)
    ttt <- paste(shortName,levelType,level,sep="\r")
    pos <- which(with(griblist,paste(shortName,indicatorOfTypeOfLevel,level,sep="\r")) == ttt)        
  } else pos <- which(griblist$shortName==shortName)
  if (!all) pos else griblist[pos,]
}

### if the grib message is in memory, not in a file:
find_in_raw <- function(msg, pattern="GRIB") {
  b <- charToRaw(pattern)
  n <- length(b)
  z <- which(msg==b[1])
  
  z[vapply(z, function(x) all(b==msg[x:(x+n-1)]), FUN.VAL=TRUE)]
}

## find a grib message in a raw data stream
grib_raw_find <- function(msg) {
  l1 <- find_in_raw(msg, "GRIB")
  l2 <- find_in_raw(msg, "7777")
  if (length(l1)==length(l2) && all(l1<l2)) {
    len1 <- vapply(l1, function(x) sum(as.integer(msg[(x+4):(x+6)])*256^(2:0)), FUN.VALUE=1)
    len2 <- l2 -l1 +4
    if (any(len1 != len2)) stop("inconsistent GRIB messages?")
    return(data.frame("begin"=l1, "end"=l2+3, "length"=len1))
  } else {
    stop("inconsistent GRIB messages?")
  }
}

grib_raw_split <- function(msg) {
  glist <- grib_raw_find(msg)
  lapply(1:dim(glist)[1], function(i) msg[glist$begin[i]:glist$end[i]])
}

Gmod <- function(gribhandle,IntPar=list(),DblPar=list(),StrPar=list(),
                 data=NULL,precision=NULL,nbits=NULL){
### modify parameters and/or data of a handle
  if (!inherits(gribhandle,"GRIBhandle")) stop("Not a GRIBhandle")
  if (length(StrPar)+length(IntPar)+length(DblPar) > 0) {
    IntPar=lapply(as.list(IntPar),as.integer)
    DblPar=lapply(as.list(DblPar),as.numeric)
    StrPar=lapply(as.list(StrPar),as.character)
    .Call("Rgrib_handle_mod",attr(gribhandle,"gribhandle_ptr"),
           StrPar,IntPar,DblPar)
  }
  if (!is.null(data)) {
    if (any(!is.finite(data))) stop("Some values are not finite! Missing values (NA) not yet supported.")
    dims <- Ginfo(gribhandle,IntPar=c("Nx","Ny","bitsPerValue",
                       "iScansNegatively","jScansPositively",
                       "jPointsAreConsecutive","alternativeRowScanning") )
    if (dims$Nx != dim(data)[1] | dims$Ny != dim(data)[2]) stop("data has wrong dimensions.")
#    if(dims$bitsPerValue==0) warning("Decimal precision is 0! Data will be constant.")
    if (dims$iScansNegatively == 1) data <- data[dims$Nx:1,]
    if (dims$jScansPositively == 0) data <- data[,dims$Ny:1]
    if (dims$alternativeRowScanning == 1) stop("alternativeRowScanning not supported")
### set the decimal precision
### if data is constant, this is reset to 0!
### for [0,1] : precision=4->14bits, 3->10bits, 5->17bits, 2->7bits
### alternatively, you may fix the number of bits per value
### if you do neither, the default is to set nbits=24
    if (!is.null(precision)) .Call("Rgrib_handle_mod",attr(gribhandle,"gribhandle_ptr"),
           StrPar=list(),IntPar=list(changeDecimalPrecision=as.integer(precision)),
           DblPar=list())
    if (!is.null(nbits)) .Call("Rgrib_handle_mod",attr(gribhandle,"gribhandle_ptr"),
           StrPar=list(),IntPar=list(bitsPerValue=as.integer(nbits)),
           DblPar=list())
    .Call("Rgrib_handle_enc",attr(gribhandle,"gribhandle_ptr"),as.numeric(as.vector(data)))
  }
  invisible(NULL)
}

Gwrite <- function(gribhandle,filename,append=TRUE){
### write a GRIBhandle to a file
  filemode <- ifelse(append,"a","w")
  filename <- path.expand(filename)
  .Call("Rgrib_handle_write",attr(gribhandle,"gribhandle_ptr"),filename,filemode)
  invisible(NULL)
}

