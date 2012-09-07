###########################################
### encode a geofield into a GRIBhandle ###
### - create new handle from sample     ###
### - modify the gridtype first !       ###
### - modify grid description           ###
### - modify meta data                  ###
### - encode the data field itself      ###
### - return a GRIBhandle               ###
###########################################

Genc=function(geofield,gribformat=2,precision=4){
  gribhandle <- Gcreate(gribformat,attributes(geofield)$domain)
  Gmod(gribhandle,IntPar=list(changeDecimalPrecision=as.integer(precision)) )
  Gmod(gribhandle,data=geofield)

### time information
#  Gmod(gribhandle,IntPar=
  gribhandle
}

Gcreate <- function(gribformat=2,DOMAIN,sample)
{
### create a new GRIBhandle
  if(missing(sample)){
    if(gribformat==1) sample="regular_ll_sfc_grib1"
    else if(gribformat==2) sample="regular_ll_sfc_grib2"
    else stop("unknown GRIB format")
  }
  cat("Creating GRIBhandle from sample ",sample,"\n")
  gribhandle <- .Call("Rgrib_handle_new_sample",sample)
  if(is.null(gribhandle)) stop("Can\'t create gribhandle.")
  class(gribhandle) <- c(class(gribhandle),"GRIBhandle")

  if(!missing(DOMAIN)){
    if(is.geofield(DOMAIN)) DOMAIN <- attributes(DOMAIN)$domain
### start building the modifications
    IntPar <- list()
    DblPar <- list()
    StrPar <- list()

### Earth shape
    if (gribformat==2 & !is.null(DOMAIN$projection$a) ){
      if( abs(DOMAIN$projection$a - 6367470.0)<10^(-5) )
        IntPar$shapeOfTheEarth <- as.integer(0)
      else if( abs(DOMAIN$projection$a - 6371229.0)<10^(-5) )
        IntPar$shapeOfTheEarth <- as.integer(6)
      else {
        IntPar$shapeOfTheEarth <- as.integer(1)
        IntPar$scaledValueOfRadiusOfSphericalEarth <- as.integer(round(DOMAIN$projection$a * 10^3))
        IntPar$scaleFactorOfRadiusOfSphericalEarth <- as.integer(3)
      }
    }

###########################
### VARIOUS PROJECTIONS ###
###########################

### LAT-LON
    if (DOMAIN$projection$proj=="latlong") {
      Gmod(gribhandle,StrPar=list(gridType="regular_ll"))

      IntPar$Nx <- DOMAIN$nx
      IntPar$Ny <- DOMAIN$ny
      if(gribformat==2) IntPar$numberOfValues  <-  DOMAIN$nx * DOMAIN$ny
      IntPar$iScansNegatively <- 0
      IntPar$jScansPositively <- 1

      DblPar$longitudeOfFirstGridPointInDegrees <- DOMAIN$SW[1]
      DblPar$latitudeOfFirstGridPointInDegrees <- DOMAIN$SW[2]
      DblPar$longitudeOfLastGridPointInDegrees <- DOMAIN$NE[1]
      DblPar$latitudeOfLastGridPointInDegrees <- DOMAIN$NE[2]
      if(is.null(DOMAIN$dx) | is.null(DOMAIN$dy)){
        DblPar$iDirectionIncrementInDegrees <- (DOMAIN$NE[1]-DOMAIN$SW[1])/(DOMAIN$nx-1)
        DblPar$jDirectionIncrementInDegrees <- (DOMAIN$NE[2]-DOMAIN$SW[2])/(DOMAIN$ny-1)
      }
      else {
        DblPar$iDirectionIncrementInDegrees <- DOMAIN$dx
        DblPar$jDirectionIncrementInDegrees <- DOMAIN$dy
      }
    }
### LAMBERT
    else if (DOMAIN$projection$proj=="lcc") {
      Gmod(gribhandle,StrPar=list(gridType="lambert"))

      IntPar$Nx <- DOMAIN$nx
      IntPar$Ny <- DOMAIN$ny
      if(gribformat==2) IntPar$numberOfValues  <-  DOMAIN$nx * DOMAIN$ny
      IntPar$iScansNegatively <- 0
      IntPar$jScansPositively <- 1
      IntPar$jPointsAreConsecutive <- 0
      IntPar$projectionCentreFlag <- 0  ### the North Pole as centre

      DblPar$Latin1InDegrees <- DOMAIN$projection$"lat_1"
      DblPar$Latin2InDegrees <- DOMAIN$projection$"lat_2"
      DblPar$LoVInDegrees <- DOMAIN$projection$"lon_0"
### This is a GUESS, may not be correct in general:
### In fact I don't really know what this LaD means...
      DblPar$LaDInDegrees <- DOMAIN$projection$"lat_1"

      DblPar$longitudeOfFirstGridPointInDegrees <- DOMAIN$SW[1]
      DblPar$latitudeOfFirstGridPointInDegrees <- DOMAIN$SW[2]

      if(is.null(DOMAIN$dx) | is.null(DOMAIN$dy)){
        xy1 <- project(DOMAIN$SW,proj=DOMAIN$projection,inv=FALSE)
        xy2 <- project(DOMAIN$NE,proj=DOMAIN$projection,inv=FALSE)

        DblPar$DxInMetres <- (xy2$x-xy1$x)/(DOMAIN$nx-1)
        DblPar$DyInMetres <- (xy2$y-xy1$y)/(DOMAIN$ny-1)
      }
      else {
        DblPar$DxInMetres <- DOMAIN$dx
        DblPar$DyInMetres <- DOMAIN$dy
      }
    }
### MERCATOR
    else if (DOMAIN$projection$proj=="merc"){
      Gmod(gribhandle,StrPar=list(gridType="mercator"))

      IntPar$Nx <- DOMAIN$nx
      IntPar$Ny <- DOMAIN$ny
      if(gribformat==2) IntPar$numberOfValues  <-  DOMAIN$nx * DOMAIN$ny
      IntPar$iScansNegatively <- 0
      IntPar$jScansPositively <- 1
      IntPar$jPointsAreConsecutive <- 0

      DblPar$LaDInDegrees <- DOMAIN$projection$lat_ts

      DblPar$longitudeOfFirstGridPointInDegrees <- DOMAIN$SW[1]
      DblPar$latitudeOfFirstGridPointInDegrees <- DOMAIN$SW[2]
      DblPar$longitudeOfLastGridPointInDegrees <- DOMAIN$NE[1]
      DblPar$latitudeOfLastGridPointInDegrees <- DOMAIN$NE[2]

      if(is.null(DOMAIN$dx) | is.null(DOMAIN$dy)){
        xy1 <- project(DOMAIN$SW,proj=DOMAIN$projection,inv=FALSE)
        xy2 <- project(DOMAIN$NE,proj=DOMAIN$projection,inv=FALSE)
        DblPar$DxInMetres <- (xy2$x-xy1$x)/(DOMAIN$nx-1)
        DblPar$DyInMetres <- (xy2$y-xy1$y)/(DOMAIN$ny-1)
      }
      else {
        DblPar$DxInMetres <- DOMAIN$dx
        DblPar$DyInMetres <- DOMAIN$dy
      }
    }
### POLAR STEROGRAPHIC
    else if (DOMAIN$projection$proj=="stere"){
      Gmod(gribhandle,StrPar=list(gridType="polar_stereographic"))

      IntPar$Nx <- DOMAIN$nx
      IntPar$Ny <- DOMAIN$ny
      if(gribformat==2) IntPar$numberOfValues  <-  DOMAIN$nx * DOMAIN$ny
      IntPar$iScansNegatively <- 0
      IntPar$jScansPositively <- 1
      IntPar$jPointsAreConsecutive <- 0

      DblPar$longitudeOfFirstGridPointInDegrees <- DOMAIN$SW[1]
      DblPar$latitudeOfFirstGridPointInDegrees <- DOMAIN$SW[2]
      DblPar$LoVInDegrees <- DOMAIN$projection$"lon_0"

      if(is.null(DOMAIN$dx) | is.null(DOMAIN$dy)){
        xy1 <- project(DOMAIN$SW,proj=DOMAIN$projection,inv=FALSE)
        xy2 <- project(DOMAIN$NE,proj=DOMAIN$projection,inv=FALSE)
        DblPar$DxInMetres <- (xy2$x-xy1$x)/(DOMAIN$nx-1)
        DblPar$DyInMetres <- (xy2$y-xy1$y)/(DOMAIN$ny-1)
      }
      else {
        DblPar$DxInMetres <- DOMAIN$dx
        DblPar$DyInMetres <- DOMAIN$dy
      }
    }

### ROTATED MERCATOR -> not yet in GRIB-2
### OBLIQUE PROJECTIONS
    else if (DOMAIN$projection$proj=="ob_tran" &
        !is.null(DOMAIN$projection$o_proj) ){
### ROTATED LAT-LON
      if(DOMAIN$projection$o_proj=="latlong") {
        Gmod(gribhandle,StrPar=list(gridType="rotated_ll"))

        IntPar$Nx <- DOMAIN$nx
        IntPar$Ny <- DOMAIN$ny
        if(gribformat==2) IntPar$numberOfValues <- DOMAIN$nx * DOMAIN$ny
        IntPar$iScansNegatively <- 0
        IntPar$jScansPositively <- 1
### RLL is defined with SW and NE in the rotated co-ordinates!
        SWr <- project(x=DOMAIN$SW[1],y=DOMAIN$SW[2],
                    proj=DOMAIN$projection,inv=FALSE)/pi*180
        NEr <- project(x=DOMAIN$NE[1],y=DOMAIN$NE[2],
                    proj=DOMAIN$projection,inv=FALSE)/pi*180

        DblPar$longitudeOfFirstGridPointInDegrees <- SWr[1]
        DblPar$latitudeOfFirstGridPointInDegrees <- SWr[2]
        DblPar$longitudeOfLastGridPointInDegrees <- NEr[1]
        DblPar$latitudeOfLastGridPointInDegrees <- NEr[2]
        if(is.null(DOMAIN$dx) | is.null(DOMAIN$dy)){
          DblPar$iDirectionIncrementInDegrees <- (NEr[1] - SWr[1])/(DOMAIN$nx-1)
          DblPar$jDirectionIncrementInDegrees <- (NEr[2] - SWr[2])/(DOMAIN$ny-1)
        }
        else {
          DblPar$iDirectionIncrementInDegrees <- DOMAIN$dx
          DblPar$jDirectionIncrementInDegrees <- DOMAIN$dy
        }
        if(DOMAIN$projection$"o_lon_p"!=0)
          stop("RotLatLon - Sorry, not implemented for some rotations...")
        DblPar$latitudeOfSouthernPoleInDegrees  <-  -DOMAIN$projection$"o_lat_p"
        DblPar$longitudeOfSouthernPoleInDegrees  <-  DOMAIN$projection$"lon_0"
        DblPar$angleOfRotationInDegrees <- 0
      }
    }
### ...
    else {
      stop("Sorry, this projection isn\'t implemented yet!")
    }
    Gmod(gribhandle,StrPar=StrPar,IntPar=IntPar,DblPar=DblPar)
  }
  gribhandle
}


