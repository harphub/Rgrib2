###########################################
### encode a geofield into a GRIBhandle ###
### - create new handle from sample     ###
### - modify the gridtype first !       ###
### - modify grid description           ###
### - modify meta data                  ###
### - encode the data field itself      ###
### - return a GRIBhandle               ###
###########################################

Genc <- function(geofield,gribformat=2,precision=4){
  gribhandle <- Gcreate(gribformat,attributes(geofield)$domain)
  Gmod(gribhandle,IntPar=list(changeDecimalPrecision=as.integer(precision)) )
  Gmod(gribhandle,data=geofield)

### time information
#  Gmod(gribhandle,IntPar=
  gribhandle
}

Gcreate <- function(gribformat=2,domain,sample)
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

  if (!missing(domain)){
    if (!inherits(domain,"geodomain")) domain <- attributes(domain)$domain
### start building the modifications
    IntPar <- list()
    DblPar <- list()
    StrPar <- list()

### Earth shape
    if (gribformat==2 & !is.null(domain$projection$a) ){
      if( abs(domain$projection$a - 6367470.0)<10^(-5) )
        IntPar$shapeOfTheEarth <- as.integer(0)
      else if( abs(domain$projection$a - 6371229.0)<10^(-5) )
        IntPar$shapeOfTheEarth <- as.integer(6)
      else {
        IntPar$shapeOfTheEarth <- as.integer(1)
        IntPar$scaledValueOfRadiusOfSphericalEarth <- as.integer(round(domain$projection$a * 10^3))
        IntPar$scaleFactorOfRadiusOfSphericalEarth <- as.integer(3)
      }
    }

###########################
### VARIOUS PROJECTIONS ###
###########################

### LAT-LON
    if (domain$projection$proj=="latlong") {
      Gmod(gribhandle,StrPar=list(gridType="regular_ll"))

      IntPar$Nx <- domain$nx
      IntPar$Ny <- domain$ny
      if(gribformat==2) IntPar$numberOfValues  <-  domain$nx * domain$ny
      IntPar$iScansNegatively <- 0
      IntPar$jScansPositively <- 1

      DblPar$longitudeOfFirstGridPointInDegrees <- domain$SW[1]
      DblPar$latitudeOfFirstGridPointInDegrees <- domain$SW[2]
      DblPar$longitudeOfLastGridPointInDegrees <- domain$NE[1]
      DblPar$latitudeOfLastGridPointInDegrees <- domain$NE[2]
      if(is.null(domain$dx) | is.null(domain$dy)){
        DblPar$iDirectionIncrementInDegrees <- (domain$NE[1]-domain$SW[1])/(domain$nx-1)
        DblPar$jDirectionIncrementInDegrees <- (domain$NE[2]-domain$SW[2])/(domain$ny-1)
      }
      else {
        DblPar$iDirectionIncrementInDegrees <- domain$dx
        DblPar$jDirectionIncrementInDegrees <- domain$dy
      }
    }
### LAMBERT
    else if (domain$projection$proj=="lcc") {
      Gmod(gribhandle,StrPar=list(gridType="lambert"))

      IntPar$Nx <- domain$nx
      IntPar$Ny <- domain$ny
      if(gribformat==2) IntPar$numberOfValues  <-  domain$nx * domain$ny
      IntPar$iScansNegatively <- 0
      IntPar$jScansPositively <- 1
      IntPar$jPointsAreConsecutive <- 0
      IntPar$projectionCentreFlag <- 0  ### the North Pole as centre

      DblPar$Latin1InDegrees <- domain$projection$"lat_1"
      DblPar$Latin2InDegrees <- domain$projection$"lat_2"
      DblPar$LoVInDegrees <- domain$projection$"lon_0"
### This is a GUESS, may not be correct in general:
### In fact I don't really know what this LaD means...
      DblPar$LaDInDegrees <- domain$projection$"lat_1"

      DblPar$longitudeOfFirstGridPointInDegrees <- domain$SW[1]
      DblPar$latitudeOfFirstGridPointInDegrees <- domain$SW[2]

      if(is.null(domain$dx) | is.null(domain$dy)){
        xy1 <- project(domain$SW,proj=domain$projection,inv=FALSE)
        xy2 <- project(domain$NE,proj=domain$projection,inv=FALSE)

        DblPar$DxInMetres <- (xy2$x-xy1$x)/(domain$nx-1)
        DblPar$DyInMetres <- (xy2$y-xy1$y)/(domain$ny-1)
      }
      else {
        DblPar$DxInMetres <- domain$dx
        DblPar$DyInMetres <- domain$dy
      }
    }
### MERCATOR
    else if (domain$projection$proj=="merc"){
      Gmod(gribhandle,StrPar=list(gridType="mercator"))

      IntPar$Nx <- domain$nx
      IntPar$Ny <- domain$ny
      if(gribformat==2) IntPar$numberOfValues  <-  domain$nx * domain$ny
      IntPar$iScansNegatively <- 0
      IntPar$jScansPositively <- 1
      IntPar$jPointsAreConsecutive <- 0

      DblPar$LaDInDegrees <- domain$projection$lat_ts

      DblPar$longitudeOfFirstGridPointInDegrees <- domain$SW[1]
      DblPar$latitudeOfFirstGridPointInDegrees <- domain$SW[2]
      DblPar$longitudeOfLastGridPointInDegrees <- domain$NE[1]
      DblPar$latitudeOfLastGridPointInDegrees <- domain$NE[2]

      if(is.null(domain$dx) | is.null(domain$dy)){
        xy1 <- project(domain$SW,proj=domain$projection,inv=FALSE)
        xy2 <- project(domain$NE,proj=domain$projection,inv=FALSE)
        DblPar$DxInMetres <- (xy2$x-xy1$x)/(domain$nx-1)
        DblPar$DyInMetres <- (xy2$y-xy1$y)/(domain$ny-1)
      }
      else {
        DblPar$DxInMetres <- domain$dx
        DblPar$DyInMetres <- domain$dy
      }
    }
### POLAR STEROGRAPHIC
    else if (domain$projection$proj=="stere"){
      Gmod(gribhandle,StrPar=list(gridType="polar_stereographic"))

      IntPar$Nx <- domain$nx
      IntPar$Ny <- domain$ny
      if(gribformat==2) IntPar$numberOfValues  <-  domain$nx * domain$ny
      IntPar$iScansNegatively <- 0
      IntPar$jScansPositively <- 1
      IntPar$jPointsAreConsecutive <- 0

      DblPar$longitudeOfFirstGridPointInDegrees <- domain$SW[1]
      DblPar$latitudeOfFirstGridPointInDegrees <- domain$SW[2]
      DblPar$LoVInDegrees <- domain$projection$"lon_0"

      if(is.null(domain$dx) | is.null(domain$dy)){
        xy1 <- project(domain$SW,proj=domain$projection,inv=FALSE)
        xy2 <- project(domain$NE,proj=domain$projection,inv=FALSE)
        DblPar$DxInMetres <- (xy2$x-xy1$x)/(domain$nx-1)
        DblPar$DyInMetres <- (xy2$y-xy1$y)/(domain$ny-1)
      }
      else {
        DblPar$DxInMetres <- domain$dx
        DblPar$DyInMetres <- domain$dy
      }
    }

### ROTATED MERCATOR -> not yet in GRIB-2
### OBLIQUE PROJECTIONS
    else if (domain$projection$proj=="ob_tran" &
        !is.null(domain$projection$o_proj) ){
### ROTATED LAT-LON
      if(domain$projection$o_proj=="latlong") {
        Gmod(gribhandle,StrPar=list(gridType="rotated_ll"))

        IntPar$Nx <- domain$nx
        IntPar$Ny <- domain$ny
        if(gribformat==2) IntPar$numberOfValues <- domain$nx * domain$ny
        IntPar$iScansNegatively <- 0
        IntPar$jScansPositively <- 1
### RLL is defined with SW and NE in the rotated co-ordinates!
        SWr <- project(x=domain$SW[1],y=domain$SW[2],
                    proj=domain$projection,inv=FALSE)/pi*180
        NEr <- project(x=domain$NE[1],y=domain$NE[2],
                    proj=domain$projection,inv=FALSE)/pi*180

        DblPar$longitudeOfFirstGridPointInDegrees <- SWr[1]
        DblPar$latitudeOfFirstGridPointInDegrees <- SWr[2]
        DblPar$longitudeOfLastGridPointInDegrees <- NEr[1]
        DblPar$latitudeOfLastGridPointInDegrees <- NEr[2]
        if(is.null(domain$dx) | is.null(domain$dy)){
          DblPar$iDirectionIncrementInDegrees <- (NEr[1] - SWr[1])/(domain$nx-1)
          DblPar$jDirectionIncrementInDegrees <- (NEr[2] - SWr[2])/(domain$ny-1)
        }
        else {
          DblPar$iDirectionIncrementInDegrees <- domain$dx
          DblPar$jDirectionIncrementInDegrees <- domain$dy
        }
        if(domain$projection$"o_lon_p"!=0)
          stop("RotLatLon - Sorry, not implemented for some rotations...")
        DblPar$latitudeOfSouthernPoleInDegrees  <-  -domain$projection$"o_lat_p"
        DblPar$longitudeOfSouthernPoleInDegrees  <-  domain$projection$"lon_0"
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


