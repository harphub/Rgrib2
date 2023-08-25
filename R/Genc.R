###########################################
### encode a geofield into a GRIBhandle ###
### - create new handle from sample     ###
### - modify the gridtype first !       ###
### - modify grid description           ###
### - modify meta data                  ###
### - encode the data field itself      ###
### - return a GRIBhandle               ###
###########################################

Genc <- function(geofield, edition=2, precision=4){
  gribhandle <- Gcreate(domain=attr(geofield, "domain"), edition=edition)
  Gmod(gribhandle, IntPar=list(changeDecimalPrecision=as.integer(precision)) )
  Gmod(gribhandle, data=geofield)

### time information
  if (!is.null(attr(geofield, "info")$time)) {
    bdate <- attr(geofield, "info")$time$basedate
    vdate <- attr(geofield, "info")$time$validdate
    ldt <- (as.numeric(vdate) - as.numeric(bdate))
    if (ldt%%3600 == 0) {
      IntPar <- list(stepUnit = 1, stepRange = ldt/3600)
    } else {
      IntPar <- list(stepUnit = 13, stepRange = ldt)
    }
    IntPar$dataDate <- as.numeric(format(bdate, "%Y%m%d"))
    IntPar$dataTime <- as.numeric(format(bdate, "%H%M"))
    # we set to "initialisation" or "forecast"
    # other choices can be done manually
    IntPar$typeOfGeneratingProcess <- if (ldt>0) 2 else 0

    Gmod(gribhandle, IntPar=IntPar)
  }
  gribhandle
}

Gcreate <- function(domain, edition=2, IntPar=list(), DblPar=list(), StrPar=list()) {
### create a new GRIBhandle adapted to a given domain
  sample <- sprintf("regular_ll_sfc_grib%i", edition)

#  cat("Creating GRIBhandle from sample ", sample, "\n")
  # NOTE: we don't use Ghandle(), because then we have to find complete path
  gribhandle <- .Call("Rgrib_handle_new_sample", sample)
  if (is.null(gribhandle)) stop("Can\'t create gribhandle.")
  class(gribhandle) <- "GRIBhandle"
  if (!missing(domain)) {
    domain <- as.geodomain(domain)
  # NOTE: - some domains may have only clonlat, others only SW and NE points
  #       - earth radius can be "a" or "R", or maybe we have non-spherical!
### start building the modifications

### Earth shape
    if (edition==2 && any(c("R", "a", "ellps") %in% names(domain$projection)) ) {
      if (!is.null(domain$projection$ellps)) {
        IntPar$shapeOfTheEarth <- switch(domain$projection$ellps,
                                         "GRS80" = 4,
                                         "WGS84" = 5,
                                         stop("Unknown ellps=", domain$projection$ellps))
      } else if (!is.null((domain$projection$R))) {
        if (abs(domain$projection$R - 6367470.0) < 10^(-5) ) IntPar$shapeOfTheEarth <- 0
        else if (abs(domain$projection$R - 6371229.0)<10^(-5) ) IntPar$shapeOfTheEarth <- 6
        # FIXME: in fact 8 is a bit different???
        else if (abs(domain$projection$R - 6371200.0)<10^(-5) ) IntPar$shapeOfTheEarth <- 8
        else {
          # a different R value. We have 4 bytes for the (integer) value, 1 byte for a scale factor
          # for now, we just go for "cm" resolution by default...
          IntPar$shapeOfTheEarth <- 1
          sc <- 0 # 3 for "mm" precision
          IntPar$scaledValueOfRadiusOfSphericalEarth <- Round(domain$projection$R * 10^sc)
          IntPar$scaleFactorOfRadiusOfSphericalEarth <- sc
        }
      } else {
        if (domain$projection$a == 6378160 &&  
            domain$projection$b == 6356775) IntPar$shapeOfTheEarth <- 2
        else {
          # 3 for a&b in km , 7 for a&b in m
          # we just choose 7
          IntPar$shapeOfTheEarth <- 7
          IntPar$scaledValueOfRadiusOfEarthMajorAxis <- round(domain$projection$a)
          IntPar$scaleFactorOfRadiusOfEarthMajorAxis <- 0
          IntPar$scaledValueOfRadiusOfEarthMinorAxis <- round(domain$projection$b)
          IntPar$scaleFactorOfRadiusOfEarthMinorAxis <- 0
        }
      }
    }

###########################
### VARIOUS PROJECTIONS ###
###########################
    if (is.null(domain$SW)) {
      dc <- DomainCorners(domain)
      domain$SW <- dc$SW
      domain$NE <- dc$NE
    }

    if (is.null(domain$dx) || is.null(domain$dy)) {
      xy1 <- meteogrid::project(domain$SW, proj=domain$projection, inv=FALSE)
      xy2 <- meteogrid::project(domain$NE, proj=domain$projection, inv=FALSE)
      domain$dx <- (xy2$x-xy1$x)/(domain$nx-1)
      domain$dy <- (xy2$y-xy1$y)/(domain$ny-1)
      if (domain$projection$proj == "ob_tran" &&
          !is.null(domain$projection$o_proj)  &&
          domain$projection$o_proj=="latlong") {
        domain$dx <- domain$dx / pi*180.
        domain$dy <- domain$dy / pi*180.
      }
    }

  # common for all projections
    IntPar$Nx <- domain$nx
    IntPar$Ny <- domain$ny
    if (edition==2) IntPar$numberOfValues  <-  domain$nx * domain$ny
    IntPar$iScansNegatively <- 0
    IntPar$jScansPositively <- 1
    IntPar$jPointsAreConsecutive <- 0

### LAT-LON
    if (domain$projection$proj=="latlong") {
      Gmod(gribhandle, StrPar=list(gridType="regular_ll"))

      DblPar$longitudeOfFirstGridPointInDegrees <- domain$SW[1]
      DblPar$latitudeOfFirstGridPointInDegrees <- domain$SW[2]
      DblPar$longitudeOfLastGridPointInDegrees <- domain$NE[1]
      DblPar$latitudeOfLastGridPointInDegrees <- domain$NE[2]
      DblPar$iDirectionIncrementInDegrees <- domain$dx
      DblPar$jDirectionIncrementInDegrees <- domain$dy

    } else if (domain$projection$proj=="lcc") {
### LAMBERT
      Gmod(gribhandle, StrPar=list(gridType="lambert"))
      # FIXME: we should be able to get Southern hemisphere also!
      IntPar$projectionCentreFlag <- 0  ### the North Pole as centre

      DblPar$Latin1InDegrees <- domain$projection$"lat_1"
      DblPar$Latin2InDegrees <- domain$projection$"lat_2"
      DblPar$LoVInDegrees <- domain$projection$"lon_0"
### This is a GUESS, may not be correct in general:
### In fact I don't really know what this LaD means...
      DblPar$LaDInDegrees <- domain$projection$"lat_1"

      DblPar$longitudeOfFirstGridPointInDegrees <- domain$SW[1]
      DblPar$latitudeOfFirstGridPointInDegrees <- domain$SW[2]

      DblPar$DxInMetres <- domain$dx
      DblPar$DyInMetres <- domain$dy

    } else if (domain$projection$proj=="merc") {
### MERCATOR
      Gmod(gribhandle, StrPar=list(gridType="mercator"))

      DblPar$LaDInDegrees <- domain$projection$lat_ts

      DblPar$longitudeOfFirstGridPointInDegrees <- domain$SW[1]
      DblPar$latitudeOfFirstGridPointInDegrees <- domain$SW[2]
      DblPar$longitudeOfLastGridPointInDegrees <- domain$NE[1]
      DblPar$latitudeOfLastGridPointInDegrees <- domain$NE[2]

      DblPar$DxInMetres <- domain$dx
      DblPar$DyInMetres <- domain$dy

    } else if (domain$projection$proj=="stere") {
### POLAR STEROGRAPHIC
      Gmod(gribhandle, StrPar=list(gridType="polar_stereographic"))

      DblPar$longitudeOfFirstGridPointInDegrees <- domain$SW[1]
      DblPar$latitudeOfFirstGridPointInDegrees <- domain$SW[2]
      DblPar$LoVInDegrees <- domain$projection$"lon_0"

      DblPar$DxInMetres <- domain$dx
      DblPar$DyInMetres <- domain$dy

    } else if (domain$projection$proj=="ob_tran" &&
        !is.null(domain$projection$o_proj) ) {
### OBLIQUE PROJECTIONS
      if (domain$projection$o_proj=="latlong") {
### ROTATED LAT-LON
        Gmod(gribhandle, StrPar=list(gridType="rotated_ll"))

### RLL is defined with SW and NE in the rotated co-ordinates!
### the projection output is in rad...
        SWr <- meteogrid::project(x=domain$SW[1], y=domain$SW[2],
                    proj=domain$projection, inv=FALSE)/pi*180
        NEr <- meteogrid::project(x=domain$NE[1], y=domain$NE[2],
                    proj=domain$projection, inv=FALSE)/pi*180

        DblPar$longitudeOfFirstGridPointInDegrees <- SWr[1]
        DblPar$latitudeOfFirstGridPointInDegrees <- SWr[2]
        DblPar$longitudeOfLastGridPointInDegrees <- NEr[1]
        DblPar$latitudeOfLastGridPointInDegrees <- NEr[2]
        DblPar$iDirectionIncrementInDegrees <- domain$dx
        DblPar$jDirectionIncrementInDegrees <- domain$dy

        if (domain$projection$"o_lon_p" != 0)
          stop("RotLatLon - Sorry, not implemented for some rotations...")
        DblPar$latitudeOfSouthernPoleInDegrees  <-  -domain$projection$"o_lat_p"
        DblPar$longitudeOfSouthernPoleInDegrees  <-  domain$projection$"lon_0"
        DblPar$angleOfRotationInDegrees <- 0
      }
    ### ROTATED MERCATOR -> not yet in GRIB-2?
    ### BUT transverse mercator is separate
    } else {
    stop("Sorry, this projection isn\'t implemented yet!")
    }
  }
  Gmod(gribhandle, StrPar=StrPar, IntPar=IntPar, DblPar=DblPar)
  gribhandle
}


