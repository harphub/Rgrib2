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

### trim whitespace from character strings
trim <- function(x) sub(pattern=" +$",repl="",x=sub(pattern="| +",repl="",x))

"print.GRIBlist" <-
function(griblist){
  cat("File",attributes(griblist)$filename,":","\n")
  cat("containing ",attributes(griblist)$nfields,"fields.","\n")
}
####################################
"Gopen" <-
function (filename,
          IntPar=c("editionNumber","dataDate","dataTime","validityDate","validityTime","Nx","Ny",
                   "table2Version","indicatorOfParameter","indicatorOfTypeOfLevel","level"),
          StrPar=c("shortName","gridType"), DblPar=c() , multi=FALSE,lextra=TRUE)
{
### FIX ME: the default choice of parameters is only OK for GRIB-1!
### passing a logical only works on recent installations, I think
### so passing multi as an integer is safer
  nmessages <- .C("Rgrib_count_messages",filename=filename,nrec=integer(1),
                 multi=as.integer(multi))$nrec

  if(is.na(nmessages)) stop("Error opening file.")

  result <- Ginfo(filename,IntPar,DblPar,StrPar,rList=as.integer(1:nmessages),multi=multi)
### a patch for tables that are missing in grib_api
##  noresult <- result[result$shortName=="unknown" & resutl$table2Version==1,]
##  if (dim(noresult)[1] > 0) {
  if(lextra) {
    if(is.element("unknown",result$shortName)){
      data(extratab)
      missing <- which(result$shortName=="unknown" & result$table2Version==1)
      zz <- match(with(result[missing,],paste(table2Version,indicatorOfParameter,sep="\r")),
                  with(extratab,paste(table2Version,indicatorOfParameter,sep="\r")))
      result$shortName[missing] <- as.character(extratab$shortName[zz])
## BUG: because of stringsAsFActors, we now got a number, not the 
## we may have created some NA's: switch them back to "unknown"
      result$shortName[which(is.na(result$shortName))] <- "unknown"
    }
# EXTRA: should we try to get "2t" etc. 
    data(specialnames)
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
                   select=position)$position
  if(length(result)==0) NA
  else result
}


###
### The main routine for getting parameters from a GRIB file or record
###
Ginfo <- function(x,...){
  UseMethod("Ginfo")
}

Ginfo.GRIBhandle <- function(gribhandle,IntPar=c(),DblPar=c(),StrPar=c()){
  if(!inherits(gribhandle,"GRIBhandle")) stop("Not a valid GRIBhandle.")
  result <- .Call("Rgrib_handle_info",attr(gribhandle,"gribhandle_ptr"),StrPar,IntPar,DblPar)
  if(length(result)>0){
    result <- data.frame(result,stringsAsFactors=FALSE)
    names(result) <- c(StrPar,DblPar,IntPar)
  }
  result
}

Ginfo.GRIBlist <- function(griblist,...){
  filename <- attributes(griblist)$filename
  Ginfo.character(filename,...)
}

Ginfo.character <- function(filename,IntPar=c(),DblPar=c(),StrPar=c(),rList=NULL,multi=FALSE){
  if(is.null(rList) ) {
    nmessages <- .C("Rgrib_count_messages",filename=filename,nmessages=integer(1),
                 multi=as.integer(multi))$nmessages
    rList <- 1:nmessages
  }
  result <- .Call("Rgrib_parse",filename,IntPar,DblPar,StrPar,
               as.integer(rList),multi=multi)
  result <- cbind(rList,data.frame(result,stringsAsFactors=FALSE))
  names(result) <- c("position",StrPar,DblPar,IntPar)
  result
}

#####################################
"Gdec" <-
function (x,field=1,get.meta=TRUE,multi=FALSE)
{
# Decode a grib record (call to C routine)
# return data
  freeHandle <- TRUE
  if(inherits(x,"GRIBhandle")) {
    gribhandle <- x
    freeHandle <- FALSE
  }
  else if(inherits(x,"GRIBlist")) {
    gribhandle <- Ghandle(attributes(x)$filename,field,multi=multi)
    if (is.null(gribhandle)) stop("Could not create GRIBhandle.")
  }
  else if(is.character(x)) {
    gribhandle <- Ghandle(x,field,multi=multi)
    if (is.null(gribhandle)) stop("Could not create GRIBhandle.")
  }
  else stop("not a valid GRIB reference")

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
    attributes(result)$domain <- Ggrid(gribhandle)
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
    tabinfo <- Ginfo(gribhandle,IntPar=c("table2Version","indicatorOfParameter",
                                         "centre","subCentre","generatingProcessIdentifier"))
    param     <- tabinfo$indicatorOfParameter
    center    <- tabinfo$centre
    subcenter <- tabinfo$subCentre
    partab    <- tabinfo$table2Version
    process   <- tabinfo$generatingProcessIdentifier

    ggg$parameterName <- Gdescribe.local(param,center,subcenter,partab,process)
  }
### return
  return(list(name=ggg$parameterName,origin=ggg$centre,
              level=ggg$level,leveltype=ggg$levelType))
}

Gdescribe.local <- function(param,center,subcenter,partab,process){
### a patch to read parameter name from local GRIB tables in stead of grib_api
### For NCEP tables, I adapted the code from wgrib (W. Ebisuzaki)
#    datapath <- paste(searchpaths()[grep("/Rgrib2$",searchpaths())],"/data/",sep="")
#    if (file.exists(paste(datapath,"tablist.csv",sep="")))
#      tablist <- read.table(file=paste(datapath,"tablist.csv",sep=""),header=TRUE,sep=";",strip.white = TRUE)
#    else if (file.exists(paste(datapath,"tablist.csv.gz",sep="")))
#      tablist <- read.table(gzfile(paste(datapath,"tablist.csv.gz",sep="")),header=TRUE,sep=";",strip.white = TRUE)
#    else {
#      warning("Could not find tablist.csv(.gz) file! Is the library installed correctly?")
#      return("unknown")
#    }
    data(tablist,package="Rgrib2",envir=environment(NULL))
    if ( (center==7) & (partab <= 3) ){
       if (subcenter == 1) loadpartable <- "ncepreanal"
       else
         if (subcenter != 0 | (process != 80 & process != 180) | (partab != 1 & partab != 2))
             loadpartable <- "ncepopn"
### default NCEP table:
         else loadpartable <- "ncepopn"
    }
    else loadpartable <-  as.character(tablist[ (tablist[,1]==center) & (tablist[,2]==partab),3])
### default table:
    if (length(loadpartable)==0 & partab==1) loadpartable <- "WMOtable001"
    if (length(loadpartable)==0) {
     warning(paste("Unknown parameter table:\n center=",center," subcenter=",
              subcenter," process=",process,"\n partable=",partab,"parameter=",param))
      return("unknown")
    }
    else{
       data(list=loadpartable,package="Rgrib2",envir=environment(NULL))
       assign("partable",eval(parse(text=loadpartable)))
#      partable <- read.table(file=paste(datapath,loadpartable,".csv",sep=""),header=TRUE,sep=";")
       result <- trim(partable$comment[!is.na(match(partable$field,param))])
       if(length(result)==0) result=paste("unknown_",loadpartable,"_",i2a(param,3),sep="")

    }

  result
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

Ghandle <- function(x,message,multi=FALSE){
### create a GRIBhandle from a file and message number
  if(inherits(x,"GRIBlist")) filename=attributes(x)$filename
  else if (is.character(x)) filename=x
  else stop("Not a valid file of GRIB reference.")

  gribhandle <- .Call("Rgrib_handle_new_file",filename,as.integer(message),multi)

  if(!is.null(gribhandle)) class(gribhandle)=c(class(gribhandle),"GRIBhandle")
  gribhandle
}

Gmod <- function(gribhandle,StrPar=list(),IntPar=list(),DblPar=list(),data,precision=NULL,nbits=NULL,...){
### modify parameters and/or data of a handle
  if(!inherits(gribhandle,"GRIBhandle")) stop("Not a GRIBhandle")
  if(length(StrPar)+length(IntPar)+length(DblPar) > 0) {
    IntPar=lapply(as.list(IntPar),as.integer)
    DblPar=lapply(as.list(DblPar),as.numeric)
    StrPar=lapply(as.list(StrPar),as.character)
    .Call("Rgrib_handle_mod",attr(gribhandle,"gribhandle_ptr"),
           StrPar,IntPar,DblPar)
  }
  if(!missing(data)) {
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

print.GRIBhandle <- function(gribhandle){
  cat("GRIBhandle (ID=",as.integer(gribhandle),")\n")
  if(!is.null(attr(gribhandle,"filename"))) {
    cat("from file",attr(gribhandle,"filename"),"\n")
    cat("message number",attr(gribhandle,"message"),"\n")
  }
  if(!is.null(attr(gribhandle,"sample"))) {
    cat("from sample",attr(gribhandle,"sample"),"\n")
  }
}

