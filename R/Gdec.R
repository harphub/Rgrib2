
"Gdec" <-
function (x, field=NULL, level=NULL, levelType="P", get.meta=TRUE, multi=FALSE, ...)
{
### FIX ME: pos should point at the position in the file
### TODO: - often, domain and time meta-data are already available from Gopen
### use field for the GRIB par number?
###       - level and levelType shoud become part of the new "keys" argument
# Decode a grib record (call to C routine)
# return data
  freeHandle <- TRUE
  if (inherits(x, "GRIBhandle")) {
    gribhandle <- x
    freeHandle <- FALSE
  } else if (is.character(field)) {
    # allow asking a field by shortName
    # this may also require providing a level (model, pressure, height...)
    # THIS WILL BE OBSOLETE SOON: message can be list of key values
    # 
    sel <- Gfind(x, shortName=field, levelType=levelType, level=level, all=TRUE)
    if (dim(sel)[1]!=1) {
      print(sel)
      stop("Need exactly 1 matching field!")
    }
    pos <- sel$position
    gribhandle <- Ghandle(x, message=pos, multi=multi)
  } else {
    gribhandle <- Ghandle(x, message=field, multi=multi)
  }
  if (is.null(gribhandle)) stop("Could not create GRIBhandle.")

  data <- .Call("Rgrib_handle_decode",attr(gribhandle,"gribhandle_ptr"))
#  cat("data length:",length(data),"\n")
  scan <- Ginfo(gribhandle,IntPar=c("Nx","Ny","iScansNegatively","jScansPositively",
                                     "jPointsAreConsecutive","alternativeRowScanning",
                                     "missingValue","numberOfMissing" ) )
  if (scan$numberOfMissing > 0) data[which(data==scan$missingValue)] <- NA
  if (is.na(scan$Nx) | is.na(scan$Ny)) {
    warning("Spectral harmonics data not yet supported")
    return(data)
  } else if (scan$Nx<=0 | scan$Ny <= 0) {
    warning("(reduced) gaussian grid is experimental!")
    N <- Ginfo(gribhandle,IntPar="N")$N
    Nggg <- paste("N",N,sep="")
    cat("N=",N,"loading",Nggg,"\n")
    data(list=Nggg,package="Rgrib2",envir=environment(NULL))
    assign("Ngg",eval(parse(text=Nggg)))

    result <- matrix(NA, ncol=2*N, nrow=4*N+1)
    print(dim(result))
    gridtype <- Ginfo(gribhandle, StrPar="gridType")$gridType
    print(gridtype)
    if (gridtype=="reduced_gg") Nlon <- Ngg$reduced else Nlon <- rep(4*N,4*N)
    i <- 1
# ECMWF: iScansNeg=0,jScansPos=0,jPointsConsec=0
# so the points start NE, go by longitude
    for (lat in (2*N):1){
      result[1:Nlon[lat],lat] <- data[i:(i+Nlon[lat]-1)]
      result[(Nlon[lat]+1),lat] <- result[1,lat] # for periodicity: much easier this way
      i <- i+Nlon[lat]
    }
    class(result) <- c(class(result),"gaussian")
  } else {
 # standard LAM grid
    result <- matrix(data, nrow=scan$Nx, ncol=scan$Ny, byrow=(scan$jPointsAreConsecutive==1))
    if (scan$iScansNegatively==1) result <- result[scan$Nx:1,]
    if (scan$jScansPositively==0) result <- result[,scan$Ny:1]
    if (scan$alternativeRowScanning == 1) warning("Alternative Row Scanning not supported!")
  }
  if (get.meta){
    ## TODO: domain & time are (usually) known from Gopen
    result <- meteogrid::as.geofield(result, domain=Gdomain(gribhandle),
                          info = c(Gdescribe(gribhandle), time=list(Gtime(gribhandle))))
  }
  # not really necessary: garbage collection does this:
  if (freeHandle) GhandleFree(gribhandle)
  result
}
"Gdec2" <-
function (x, field=1, level=NULL, levelType="P", get.meta=TRUE, multi=FALSE, ...)
{
### FIX ME: pos should point at the position in the file
### TODO: often, domain and time meta-data are already available from Gopen
### use field for the GRIB par number?
# Decode a grib record (call to C routine)
# return data
  freeHandle <- TRUE
  if (inherits(x, "GRIBhandle")) {
    gribhandle <- x
    freeHandle <- FALSE
  } else if (is.character(field)) {
    # allow asking a field by shortName
    # this may also require providing a level (model, pressure, height...)
    sel <- Gfind(x, shortName=field, levelType=levelType, level=level, all=TRUE)
    if (dim(sel)[1]!=1) {
      print(sel)
      stop("Need exactly 1 matching field!")
    }
    pos <- sel$position
    gribhandle <- Ghandle(x, pos, multi=multi)
  } else {
    # x is a raw message or a GRIBlist
    gribhandle <- Ghandle(x, field, multi=multi)
  }
  if (is.null(gribhandle)) stop("Could not create GRIBhandle.")

  result <- .Call("Rgrib_handle_decode_2", attr(gribhandle, "gribhandle_ptr"))

  if (get.meta){
    ## TODO: domain & time are (usually) known from Gopen
    result <- meteogrid::as.geofield(result, domain=Gdomain(gribhandle),
                          info = c(Gdescribe(gribhandle), time=list(Gtime(gribhandle))))
  }
  # not really necessary: garbage collection does this:
  if (freeHandle) GhandleFree(gribhandle)
  result
}


"Gdec3" <-
function (x, field=1, level=NULL, levelType="P", get.meta=TRUE, multi=FALSE, ...)
{
### FIX ME: pos should point at the position in the file
### TODO: often, domain and time meta-data are already available from Gopen
### use field for the GRIB par number?
# Decode a grib record (call to C routine)
# return data
  freeHandle <- TRUE
  if (inherits(x, "GRIBhandle")) {
    gribhandle <- x
    freeHandle <- FALSE
  } else if (is.character(field)) {
    # allow asking a field by shortName
    # this may also require providing a level (model, pressure, height...)
    sel <- Gfind(x, shortName=field, levelType=levelType, level=level, all=TRUE)
    if (dim(sel)[1]!=1) {
      print(sel)
      stop("Need exactly 1 matching field!")
    }
    pos <- sel$position
    gribhandle <- Ghandle(x, pos, multi=multi)
  } else {
    # x is a raw message or a GRIBlist
    gribhandle <- Ghandle(x, field, multi=multi)
  }
  if (is.null(gribhandle)) stop("Could not create GRIBhandle.")

  data <- .Call("Rgrib_handle_decode",attr(gribhandle,"gribhandle_ptr"))
#  cat("data length:",length(data),"\n")
  scan <- Ginfo(gribhandle,IntPar=c("Nx","Ny","iScansNegatively","jScansPositively",
                                     "jPointsAreConsecutive","alternativeRowScanning",
                                     "missingValue","numberOfMissing" ) )
  if (scan$numberOfMissing > 0) data[which(data==scan$missingValue)] <- NA
  if (is.na(scan$Nx) | is.na(scan$Ny)) {
    warning("Spectral harmonics data not yet supported")
    return(data)
  } else if (scan$Nx<=0 | scan$Ny <= 0) {
    warning("(reduced) gaussian grid is experimental!")
    N <- Ginfo(gribhandle,IntPar="N")$N
    Nggg <- paste("N",N,sep="")
    cat("N=",N,"loading",Nggg,"\n")
    data(list=Nggg,package="Rgrib2",envir=environment(NULL))
    assign("Ngg",eval(parse(text=Nggg)))

    result <- matrix(NA, ncol=2*N, nrow=4*N+1)
    print(dim(result))
    gridtype <- Ginfo(gribhandle, StrPar="gridType")$gridType
    print(gridtype)
    if (gridtype=="reduced_gg") Nlon <- Ngg$reduced else Nlon <- rep(4*N,4*N)
    i <- 1
# ECMWF: iScansNeg=0,jScansPos=0,jPointsConsec=0
# so the points start NE, go by longitude
    for (lat in (2*N):1){
      result[1:Nlon[lat],lat] <- data[i:(i+Nlon[lat]-1)]
      result[(Nlon[lat]+1),lat] <- result[1,lat] # for periodicity: much easier this way
      i <- i+Nlon[lat]
    }
    class(result) <- c(class(result),"gaussian")
  } else {
 # standard LAM grid
    result <- matrix(data, nrow=scan$Nx, ncol=scan$Ny, byrow=(scan$jPointsAreConsecutive==1))
    if (scan$iScansNegatively==1) result <- result[scan$Nx:1,]
    if (scan$jScansPositively==0) result <- result[,scan$Ny:1]
    if (scan$alternativeRowScanning == 1) warning("Alternative Row Scanning not supported!")
  }
  if (get.meta){
    ## TODO: domain & time are (usually) known from Gopen
    ##       but e.g. lead time MAY vary inside a GRIB file (not common, but possible)
    result <- meteogrid::as.geofield(result, domain=Gdomain(gribhandle),
                          info = c(Gdescribe(gribhandle), time=list(Gtime(gribhandle))))
  }
  # not really necessary: garbage collection does this:
  if (freeHandle) GhandleFree(gribhandle)
  result
}
