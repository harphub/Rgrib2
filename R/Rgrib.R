#####################################
### Rgrib : read GRIB files into R
### Alex Deckmyn, Royal Meteorological Institute, Belgium
### alex.deckmyn@oma.be
###
### long set of non-listed of modifications
### 14/07/2006 : long overdue clean-up of Gdescribe, add DWD-204 for PEPS
### 14/08/2006 : Inclusion of scanning mode and wind direction flags!
### 03/10/2006 : Added a "raw" switch to Gdec to read th edata "as is"
###              Useful for difference files with plain Fourier components
### 08/01/2008 : added rotated LatLon grid (for Hirlam domains)
###              added some more "table 2" versions
### ../10/2009 : start port to GRIB_API
### 27/10/2010 : First version linked to grib_api
### 21/01/2011 : Read local table-2 for unknown parameters.
### ../09/2011 : Change in Gmod for data: allow either precision or nbits
### ../09/2012 : MULTI messages support -- BUG in GRIB_API, so not very useful
#####################################

"print.GRIBlist" <-
function(x,...){
  cat("File",attributes(x)$filename,":","\n")
  cat("containing ",attributes(x)$nfields,"fields.","\n")
}
####################################
"Gopen" <-
function (filename,
          IntPar=c("editionNumber","dataDate","dataTime","validityDate","validityTime","Nx","Ny",
                   "table2Version","indicatorOfParameter","indicatorOfTypeOfLevel","level"),
          DblPar=c(), StrPar=c("shortName","gridType"), multi=FALSE,lextra=TRUE)
{
### FIX ME: the default choice of parameters is only OK for GRIB-1!
### passing a logical only works on recent installations, I think
### so passing multi as an integer is safer
  filename <- path.expand(filename)
  nmessages <- .C("Rgrib_count_messages",filename=filename,nrec=integer(1),
                 multi=as.integer(multi))$nrec

  if(is.na(nmessages)) stop("Error opening file.")

  result <- Ginfo(filename,IntPar,DblPar,StrPar,rList=as.integer(1:nmessages),multi=multi)
### a patch for tables that are missing in grib_api
##  noresult <- result[result$shortName=="unknown" & resutl$table2Version==1,]
##  if (dim(noresult)[1] > 0) {
  if (lextra) {
    if (is.element("unknown",result$shortName)) {
      extratab <- get("extratab")
      missing <- which(result$shortName=="unknown")
      zz <- match(with(result[missing,],paste(table2Version,indicatorOfParameter,sep="\r")),
                  with(extratab,paste(table2Version,indicatorOfParameter,sep="\r")))
### use as.character to fix for default stringsAsFactors in data()...
      result$shortName[missing] <- as.character(extratab$shortName[zz])
## we may have created some NA's: switch them back to "unknown"
      result$shortName[which(is.na(result$shortName))] <- "unknown"
    }
# EXTRA: should we try to get "2t" etc. 
    specialnames <- get("specialnames")
    zz2 <- match(with(result,paste(table2Version,indicatorOfParameter,indicatorOfTypeOfLevel,level,sep="\r")),
                 with(specialnames,paste(table2Version,indicatorOfParameter,indicatorOfTypeOfLevel,level,sep="\r")))
    zz3 <- which(!is.na(zz2))
    if(length(zz3)>0) result$shortName[zz3] <- as.character(specialnames$shortName[zz2[zz3]])

  }
###
  attributes(result)$filename <- filename
  attributes(result)$nmessages <- nmessages
  class(result) <- c(class(result),"GRIBlist")
  result
}

################################
### find a specific message
### this is the "easy but slow" implementation: first get a complete table and then search in R
Glocate <- function(filename,IntPar=list(),DblPar=list(),StrPar=list(),...){
  gribtab <- Ginfo(filename,IntPar=names(IntPar),DblPar=names(DblPar),StrPar=names(StrPar),
                   rList=NULL,...)
  allpar <- c(IntPar,DblPar,StrPar)
  subs <- paste(names(allpar),"==\"",allpar,"\"",sep="",collapse=" & ")
  result <- subset(gribtab,subset=eval(parse(text=subs)),
                   select="position")$position   # use quotes to satisfy "check"
  if(length(result)==0) NA
  else result
}

### a more basic version for parameter (number or shortName), 
### leave parameter list "open" (,...) for later additions
Gfind <- function(griblist,shortName="t",level=NULL,levelType="P",all=FALSE){
  if(is.character(griblist)) griblist <- Gopen(griblist)
  if(!is.null(level)){
    levelType <- switch(levelType,
              "P"=100,
              "H"=105,
              "S"=109,
              levelType)
    ttt <- paste(shortName,levelType,level,sep="\r")
    pos <- which(with(griblist,paste(shortName,indicatorOfTypeOfLevel,level,sep="\r")) == ttt)        
  }
  else pos <- which(griblist$shortName==shortName)
  if(!all) pos
  else griblist[pos,]
}

###
### The main routine for getting parameters from a GRIB file or record
###
Ginfo <- function(x,...){
  UseMethod("Ginfo")
}

Ginfo.GRIBhandle <- function(x,IntPar=c(),DblPar=c(),StrPar=c(),...){
  result <- .Call("Rgrib_handle_info",attr(x,"gribhandle_ptr"),StrPar,IntPar,DblPar)
  if(length(result)>0){
    result <- data.frame(result,stringsAsFactors=FALSE)
    names(result) <- c(StrPar,DblPar,IntPar)
  }
  result
}

Ginfo.GRIBlist <- function(x,IntPar=c(),DblPar=c(),StrPar=c(),rList=NULL,multi=FALSE,...){
  filename <- attributes(x)$filename
  Ginfo.character(filename,IntPar,DblPar,StrPar,rList,multi)
}

Ginfo.character <- function(x,IntPar=c(),DblPar=c(),StrPar=c(),rList=NULL,multi=FALSE,...){
  if(is.null(rList) ) {
    nmessages <- .C("Rgrib_count_messages",filename=x,nmessages=integer(1),
                 multi=as.integer(multi))$nmessages
    rList <- 1:nmessages
  }
  filename <- path.expand(x)
  result <- .Call("Rgrib_parse",filename,IntPar,DblPar,StrPar,
               as.integer(rList),multi=multi)
  result <- cbind(rList,data.frame(result,stringsAsFactors=FALSE))
  names(result) <- c("position",StrPar,DblPar,IntPar)
  result
}

#####################################
"Gdec" <-
function (x,field=1,level=NULL,levelType="P",get.meta=TRUE,multi=FALSE)
{
### FIX ME: pos should point at the position in the file
### use field for the GRIB par number?
# Decode a grib record (call to C routine)
# return data
  freeHandle <- TRUE
  if(inherits(x,"GRIBhandle")) {
    gribhandle <- x
    freeHandle <- FALSE
  }
### allow asking a field by shortName
### this may also require providing a level (model, pressure, height...)
  else if(is.character(field)){
    sel <- Gfind(x,shortName=field,levelType=levelType,level=level,all=TRUE)
    if(dim(sel)[1]!=1) {
      print(sel)
      stop("Need exactly 1 matching field!")
    }
    pos <- sel$position
    gribhandle <- Ghandle(x,pos,multi=multi)
  }
  else {
    gribhandle <- Ghandle(x,field,multi=multi)
  }
  if (is.null(gribhandle)) stop("Could not create GRIBhandle.")

  data <- .Call("Rgrib_handle_decode",attr(gribhandle,"gribhandle_ptr"))
#  cat("data length:",length(data),"\n")
  scan <- Ginfo(gribhandle,IntPar=c("Nx","Ny","iScansNegatively","jScansPositively",
                                     "jPointsAreConsecutive","alternativeRowScanning",
                                     "missingValue","numberOfMissing" ) )
  if(scan$numberOfMissing > 0) data[which(data==scan$missingValue)] <- NA
  if(is.na(scan$Nx) | is.na(scan$Ny)) {
    warning("Spectral harmonics data not yet supported")
    return(data)
  }
  else if(scan$Nx<=0 | scan$Ny <= 0){
    warning("(reduced) gaussian grid is experimental!")
    N <- Ginfo(gribhandle,IntPar="N")$N
    Nggg <- paste("N",N,sep="")
    cat("N=",N,"loading",Nggg,"\n")
    data(list=Nggg,package="Rgrib2",envir=environment(NULL))
    assign("Ngg",eval(parse(text=Nggg)))

    result <- matrix(NA,ncol=2*N,nrow=4*N+1)
    print(dim(result))
    gridtype <- Ginfo(gribhandle,StrPar="gridType")$gridType
    print(gridtype)
    if(gridtype=="reduced_gg") Nlon <- Ngg$reduced
    else Nlon <- rep(4*N,4*N)
    i <- 1
# ECMWF: iScansNeg=0,jScansPos=0,jPointsConsec=0
# so the points start NE, go by longitude
    for(lat in (2*N):1){
      result[1:Nlon[lat],lat] <- data[i:(i+Nlon[lat]-1)]
      result[(Nlon[lat]+1),lat] <- result[1,lat] # for periodicity: much easier this way
      i <- i+Nlon[lat]
    }
    class(result) <- c(class(result),"gaussian")
  }
  else {
 # standard LAM grid
    result <- matrix(data,nrow=scan$Nx,ncol=scan$Ny,byrow=(scan$jPointsAreConsecutive==1))
    if(scan$iScansNegatively==1) result <- result[scan$Nx:1,]
    if(scan$jScansPositively==0) result <- result[,scan$Ny:1]
    if(scan$alternativeRowScanning == 1) warning("Alternative Row Scanning not supported!")
  }
  if(get.meta){
    attributes(result)$domain <- Gdomain(gribhandle)
    attributes(result)$info <- Gdescribe(gribhandle)
    attributes(result)$time <- Gtime(gribhandle)
    class(result) <- c(class(result),"geofield")
  }
  if(freeHandle)GhandleFree(gribhandle)  # not really necessary: garbage collection does this
  result
}
####################################
"Gdescribe" <- function(gribhandle)
{
  ggg <- Ginfo(gribhandle,
          StrPar=c("centre","subCentre","parameterName","levelType","name"),
          IntPar=c("level","editionNumber")
         )
### a temporary fix for unconventional tables
  if(ggg$name=="unknown" & ggg$editionNumber==1){
    extratab <- get("extratab")
    zz <- match(paste(ggg$table2Version,ggg$indicatorOfParameter,sep="\r"),
                with(extratab,paste(table2Version,indicatorOfParameter,sep="\r")))
    if (!is.na(zz)) gg$parameterName <- as.character(extratab$name[zz])
  }
### return
  return(list(name=ggg$parameterName,origin=ggg$centre,
              level=ggg$level,leveltype=ggg$levelType))
}

#####################################
Gtime <- function(gribhandle,...)
{
  ggg <- Ginfo(gribhandle,IntPar=c("startStep","endStep","timeRangeIndicator"),
            StrPar=c("dataDate","dataTime","stepUnits"),...)
### Initial date
### this is why it's best to ask dataDate as a string, not integer
#  initdate <- as.Date(ggg$dataDate,"%Y%m%d")

  inityear <- substring(ggg$dataDate,1,4)
  initmonth<- substring(ggg$dataDate,5,6)
  initday  <- substring(ggg$dataDate,7,8)
  inithour <- substring(ggg$dataTime,1,2)
  initmin  <- substring(ggg$dataTime,3,4)
# for backward compatibility (temporary)
  anatime <- paste(inityear,"/",initmonth,"/",initday," z",inithour,":",initmin,sep="")

### Is it a forecast or what...

  if(ggg$timeRangeIndicator==10) fcrange <- paste("+",ggg$startStep,ggg$stepUnits,sep="")
  else fcrange <- paste(ggg$startStep,"-",ggg$endStep," ",ggg$stepUnits,sep="")

  paste(anatime,fcrange)
}

Glevel <- function(gribhandle,...)
{
  ggg <- Ginfo(gribhandle,IntPar=c("indicatorOfTypeOfLevel","topLevel",
            "bottomLevel"),
            StrPar=c("stepUnits"),...)
}


#########################################
### GRIB handle -> use external pointers
#########################################

Ghandle <- function(x,message=1,multi=FALSE){
### create a GRIBhandle from a file and message number
  if(inherits(x,"GRIBlist")) filename <- attributes(x)$filename
  else if (is.character(x)) filename <- path.expand(x)
  else stop("Not a valid file of GRIB reference.")

  gribhandle <- .Call("Rgrib_handle_new_file",filename,as.integer(message),multi)

  if(!is.null(gribhandle)) class(gribhandle) <- c(class(gribhandle),"GRIBhandle")
  gribhandle
}

Gmod <- function(gribhandle,IntPar=list(),DblPar=list(),StrPar=list(),
                 data=NULL,precision=NULL,nbits=NULL){
### modify parameters and/or data of a handle
  if(!inherits(gribhandle,"GRIBhandle")) stop("Not a GRIBhandle")
  if(length(StrPar)+length(IntPar)+length(DblPar) > 0) {
    IntPar=lapply(as.list(IntPar),as.integer)
    DblPar=lapply(as.list(DblPar),as.numeric)
    StrPar=lapply(as.list(StrPar),as.character)
    .Call("Rgrib_handle_mod",attr(gribhandle,"gribhandle_ptr"),
           StrPar,IntPar,DblPar)
  }
  if(!is.null(data)) {
    if(any(!is.finite(data))) stop("Some values are not finite! Missing values (NA) not yet supported.")
    dims <- Ginfo(gribhandle,IntPar=c("Nx","Ny","bitsPerValue",
                       "iScansNegatively","jScansPositively",
                       "jPointsAreConsecutive","alternativeRowScanning") )
    if(dims$Nx != dim(data)[1] | dims$Ny != dim(data)[2]) stop("data has wrong dimensions.")
#    if(dims$bitsPerValue==0) warning("Decimal precision is 0! Data will be constant.")
    if(dims$iScansNegatively == 1) data <- data[dims$Nx:1,]
    if(dims$jScansPositively == 0) data <- data[,dims$Ny:1]
    if(dims$alternativeRowScanning == 1) stop("alternativeRowScanning not supported")
### set the decimal precision
### if data is constant, this is reset to 0!
### for [0,1] : precision=4->14bits, 3->10bits, 5->17bits, 2->7bits
### alternatively, you may fix the number of bits per value
### if you do neither, the default is to set nbits=24
    if(!is.null(precision)) .Call("Rgrib_handle_mod",attr(gribhandle,"gribhandle_ptr"),
           StrPar=list(),IntPar=list(changeDecimalPrecision=as.integer(precision)),
           DblPar=list())
    if(!is.null(nbits)) .Call("Rgrib_handle_mod",attr(gribhandle,"gribhandle_ptr"),
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

### Admin

GhandleFree <- function(gribhandle){
  if(!inherits(gribhandle,"GRIBhandle")) stop("Not a GRIBhandle.")
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
  if(!is.null(attr(x,"filename"))) {
    cat("from file",attr(x,"filename"),"\n")
    cat("message number",attr(x,"message"),"\n")
  }
  if(!is.null(attr(x,"sample"))) {
    cat("from sample",attr(x,"sample"),"\n")
  }
}

