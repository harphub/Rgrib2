"Ggrid" <- function(gribhandle){
#####################################################
### input is a GRIBhandle
### output is a "geodomain" with grid description
#####################################################
### GRIB_API  up to 1.9.5 knows scaleFactorOfEarthMajorAxis
### 1.9.9. also knows 
### tolerance for (Nx-1)*dx ~ Lx
  LonEps <- 10^(-5)

  gridtype <- Ginfo(gribhandle,StrPar="gridType")$gridType

  earthshape <- Ginfo(gribhandle,IntPar=c("editionNumber","shapeOfTheEarth","scaledValueOfRadiusOfSphericalEarth",
                         "scaleFactorOfRadiusOfSphericalEarth",
                         "scaleFactorOfEarthMajorAxis",
                         "scaledValueOfEarthMajorAxis",
                         "scaleFactorOfEarthMinorAxis",
                         "scaledValueOfEarthMinorAxis"
 ))
  
  earthproj <- switch(as.character(earthshape$shapeOfTheEarth),
  "0"=list(a=6367470.0,es=0.0), ### WMO standard for grib-1!
  "1"=list(a=10^-earthshape$scaleFactorOfRadiusOfSphericalEarth * earthshape$scaledValueOfRadiusOfSphericalEarth,es=0.0),
  "2"=list(a=6378160.0,b=6356775.0),
  "3"=list(a=10^(3-earthshape$scaleFactorOfEarthMajorAxis) * 
           earthshape$scaledValueOfEarthMajorAxis, 
         b=10^(3-earthshape$scaleFactorOfEarthMinorAxis) * 
           earthshape$scaledValueOfEarthMinorAxis),
  "4"=list(ellps="GRS80"),
  "5"=list(ellps="WGS84"),
  "6"=list(a=6371229.0,es=0.0), ### as used e.g. by ALADIN
  "7"=list(a=10^(-earthshape$scaleFactorOfEarthMajorAxis) * 
             earthshape$scaledValueOfEarthMajorAxis,
           b=10^(-earthshape$scaleFactorOfEarthMinorAxis) * 
             earthshape$scaledValueOfEarthMinorAxis),
  list(a=6371200.0,es=0.0)
  )
  if(earthshape$shapeOfTheEarth>=8) warning(paste("This earth shape is not yet fully implemented. Defaulting to sphere with radius",6371200.0)) 

### ATTENTION: the WMO standard for GRIB-1 files has earth radius a=6367470.0
###            On the other hand, NCEP uses a=6371200.0 (also hardcoded in grads)
###            and the ALADIN model a=6371229.0
###            In GRIB-1 it is not possible to pass the Earth radius.
### strangely, grib_api returns shape="6" and radius 6367470.0 for grib-1 files. This is inconsistent!
### 
### For lat/lon, this is of no consequence, but for Lambert and Mercator, 
### the earth radius has an impact when calculating NE point from SW.
### We could *assume* that Lambert projections always originate from ALADIN/ALARO/AROME... or use centre of origin?


# LATLON
  if(gridtype=="regular_ll"){
    info <- gridtype
    projection <- list(proj="latlong")

    ggg <- Ginfo(gribhandle,
              IntPar=c("Nx","Ny",
                       "iScansNegatively","jScansPositively",
                       "jPointsAreConsecutive","alternativeRowScanning"),
              DblPar=c("latitudeOfFirstGridPointInDegrees","longitudeOfFirstGridPointInDegrees",
                       "latitudeOfLastGridPointInDegrees","longitudeOfLastGridPointInDegrees",
                       "iDirectionIncrementInDegrees","jDirectionIncrementInDegrees")
              )
### Read the corners of the domain.
### Make sure to read them correctly
### e.g. if the data is ordered N->S
### is there a difference in Grib 1 and 2?
### it should be 10^3 for G1
    if (ggg$iScansNegatively) {
      Lon2 <- ggg$longitudeOfFirstGridPointInDegrees
      Lon1 <- ggg$longitudeOfLastGridPointInDegrees
    }
    else {
      Lon1 <- ggg$longitudeOfFirstGridPointInDegrees
      Lon2 <- ggg$longitudeOfLastGridPointInDegrees
    }
### set longitudes to ]-180,180]
    Lon1 <- Lon1%%360
    if(Lon1>180) Lon1 <- Lon1 - 360
    Lon2 <- Lon2%%360
    if(Lon2>180) Lon2 <- Lon2 - 360
### ...unless the domain crosses the 180/-180 meridian:
### in that case, we want it to be somewhere within ]-360,360[
### but we will have to change the central meridian of the map!
### for this we define projection$lon0
### This is used to "project" points to ]lon0-180,lon0+180]
### usually, we would expect lon0 to be 0 or +/- 180, but other values may be possible
    if (Lon1 > Lon2) {
      if (Lon2 > 0) Lon1 <- Lon1 - 360
      else  Lon2 <- Lon2 + 360

      if (Lon1>=0 ) lon0 <- 180
      else if ( Lon2<=0) lon0 <- -180
      else lon0 <- (Lon1+Lon2)/2
    }
    else lon0 <- 0
    projection$lon0 <- lon0

    if(abs((Lon2-Lon1)/(ggg$Nx-1) - ggg$iDirectionIncrementInDegrees) > LonEps){
          warning(paste("Longitudes inconsistent: Lon1=",Lon1,"Lon2=",Lon2,
                        "Nx=",ggg$Nx,"Dx=",ggg$iDirectionIncrementInDegrees))
### In fact, this usually means that the value given in the file is rounded
          delx <- (Lon2-Lon1)/(ggg$Nx-1)
    }
    else delx <- ggg$iDirectionIncrementInDegrees
### FIX ME
#    if (Lon1 + (ggg$nx +1) * delx == Lon2) {
#       periodic <- TRUE
#     }
### CELL-CENTERED GRID : meridian is at Lon1-delx/2 + 180
### VERTEX-CENTERED GRID : meridian is at Lon1 + 180
#       lon0 <- Lon1 + 180
### lon0 <- (Lon1 + Lon2 + delx)/2
### if you draw a CELL CENTERED map (image), you go to lon1-dx/2, so the actual meridian of the map is
### this is important to make sure the (global) map ends up right!
### This should better be done when calculating the Domain extent
###    projection$lon0 <- lon0 - delx/2

    if (ggg$jScansPositively) {
      Lat1 <- ggg$latitudeOfFirstGridPointInDegrees
      Lat2 <- ggg$latitudeOfLastGridPointInDegrees
    }
    else{
      Lat2 <- ggg$latitudeOfFirstGridPointInDegrees
      Lat1 <- ggg$latitudeOfLastGridPointInDegrees
    }
    if (Lat1 > Lat2) warning(paste("Inconsistent Lat1=",Lat1,"Lat2=",Lat2))
    if (abs( (Lat2-Lat1)/(ggg$Ny-1) - ggg$jDirectionIncrementInDegrees) > LonEps){
          warning(paste("Latitudes inconsistent: Lat1=",Lat1,"Lat2=",Lat2,
                        "Ny=",ggg$Ny,"Dy=",ggg$jDirectionIncrementInDegrees))
          dely <- (Lat2-Lat1)/(ggg$Ny-1)
    }
    else dely <- ggg$jDirectionIncrementInDegrees

    SW <- c(Lon1,Lat1)
    NE <- c(Lon2,Lat2)

    result <- list(projection=projection,nx=ggg$Nx,ny=ggg$Ny,SW=SW,NE=NE,
                   dx=delx, dy=dely )
    class(result) <- "geodomain"
    result

  }
### LAMBERT
else if(gridtype=="lambert"){
    info <- gridtype

    ggg <- Ginfo(gribhandle,
              IntPar=c("Nx","Ny",
                       "iScansNegatively","jScansPositively",
                       "jPointsAreConsecutive","alternativeRowScanning"),
              DblPar=c("Latin1InDegrees","Latin2InDegrees","LoVInDegrees",
                       "DxInMetres","DyInMetres",
                       "latitudeOfFirstGridPointInDegrees",
                       "longitudeOfFirstGridPointInDegrees")
           )
        La1 <-  ggg$latitudeOfFirstGridPointInDegrees
        Lo1 <-  ggg$longitudeOfFirstGridPointInDegrees
        delx <- ggg$DxInMetres
        dely <- ggg$DyInMetres
        nx <- ggg$Nx
        ny <- ggg$Ny

        if(Lo1 > 180) Lo1 <- Lo1-360

        rlat1 <-  ggg$Latin1InDegrees
        rlat2 <-  ggg$Latin2InDegrees
        rlon <-  ggg$LoVInDegrees

        projection <- c(list(proj="lcc",lon_0=rlon,lat_1=rlat1,lat_2=rlat2),earthproj)

        xy <- project(c(Lo1,La1), proj = projection,inv=FALSE)
        x0 <- xy$x[1]
        y0 <- xy$y[1]
        x1 <- x0+(nx-1)*delx
        y1 <- y0+(ny-1)*dely
        xy <- project(c(x1,y1), proj = projection,inv=TRUE)
        NE <- c(xy$x,xy$y)
        SW <- c(Lo1,La1)
        result <- list(projection=projection,nx=nx,ny=ny,SW=SW,NE=NE,dx=delx,dy=dely )
        class(result) <- "geodomain"
        result
        }
### POLAR STEREOGRAPHIC
else if(gridtype=="polar_stereographic"){ ### 20
    info <- gridtype

    ggg <- Ginfo(gribhandle,
              IntPar=c("Nx","Ny",
                       "iScansNegatively","jScansPositively",
                       "jPointsAreConsecutive","alternativeRowScanning"),
              DblPar=c("LoVInDegrees",
                       "latitudeOfFirstGridPointInDegrees",
                       "longitudeOfFirstGridPointInDegrees",
                       "DxInDegrees","DyInDegrees")
              )
        La1 <-  ggg$latitudeOfFirstGridPointInDegrees
        Lo1 <-  ggg$longitudeOfFirstGridPointInDegrees
        Lo1 <- Lo1 %% 360
        if (Lo1 > 180) Lo1 <- Lo1 - 360

        nx <- ggg$Nx
        ny <- ggg$Ny

        rlon <-  ggg$LoVInDegrees

        delx <- ggg$DxInMetres
        dely <- ggg$DyInMetres

        projection <- c(list(proj="stere",lon_0=rlon,lat_0=90),earthproj)

        xy <- project(x=Lo1,y=La1, proj = projection,inv=FALSE)
        x0 <- xy$x[1]
        y0 <- xy$y[1]
        x1 <- x0+(nx-1)*delx
        y1 <- y0+(ny-1)*dely
        xy <- project(list(x=x1,y=y1), proj = projection,inv=TRUE)
        NE <- c(xy$x,xy$y)
        SW <- c(Lo1,La1)
        result <- list(projection=projection,nx=nx,ny=ny,SW=SW,NE=NE,dx=delx,dy=dely )
        class(result) <- "geodomain"
        result
        }
### MERCATOR
else if(gridtype=="mercator"){
    info <- gridtype

    ggg <- Ginfo(gribhandle,
              IntPar=c("Nx","Ny",
                       "iScansNegatively","jScansPositively",
                       "jPointsAreConsecutive","alternativeRowScanning"),
              DblPar=c("latitudeOfFirstGridPointInDegrees","longitudeOfFirstGridPointInDegrees",
                       "DxInMetres","DyInMetres","LaDInDegrees")
              )
        La1 <-  ggg$latitudeOfFirstGridPointInDegrees
        Lo1 <-  ggg$longitudeOfFirstGridPointInDegrees
        if(Lo1 > 180) Lo1 <- Lo1-360

        nx <- ggg$Nx
        ny <- ggg$Ny

        delx <- ggg$DxInMetres
        dely <- ggg$DyInMetres

        rlat <- ggg$LaDInDegrees

        projection <- c(list(proj="merc",lat_ts=rlat),earthproj)

        xy <- project(list(x=Lo1,y=La1), proj = projection,inv=FALSE)
        x0 <- xy$x[1]
        y0 <- xy$y[1]
        x1 <- x0+(nx-1)*delx
        y1 <- y0+(ny-1)*dely
        xy <- project(list(x=x1,y=y1), proj = projection,inv=TRUE)
        NE <- c(xy$x,xy$y)
        SW <- c(Lo1,La1)
### FIX ME: for global data you should also set the central meridian lon_0 !
### this is either Lon1 + 180 or Lon1 -dx/2 + 180
        result <- list(projection=projection,nx=nx,ny=ny,SW=SW,NE=NE,dx=delx,dy=dely )
        class(result) <- "geodomain"
        result

               }
### ROTATED LATLON (for Hirlam & GLAMEPS output)
else if(gridtype=="rotated_ll") {
    info <- gridtype
    ggg <- Ginfo(gribhandle,
              IntPar=c("Nx","Ny",
                       "iScansNegatively","jScansPositively",
                       "jPointsAreConsecutive","alternativeRowScanning"),
              DblPar=c("latitudeOfFirstGridPointInDegrees",
                       "longitudeOfFirstGridPointInDegrees",
                       "latitudeOfLastGridPointInDegrees",
                       "longitudeOfLastGridPointInDegrees",
                       "iDirectionIncrementInDegrees","jDirectionIncrementInDegrees",
                       "angleOfRotationInDegrees",
                       "latitudeOfSouthernPoleInDegrees","longitudeOfSouthernPoleInDegrees")
              )
    nx <- ggg$Nx
    ny <- ggg$Ny
### Read the corners of the domain.
### Make sure to read them correctly
### e.g. if the data is ordered N->S
    if (ggg$iScansNegatively) {
      Lon2 <- ggg$longitudeOfFirstGridPointInDegrees
      Lon1 <- ggg$longitudeOfLastGridPointInDegrees
    }
    else
      {
      Lon1 <- ggg$longitudeOfFirstGridPointInDegrees
      Lon2 <- ggg$longitudeOfLastGridPointInDegrees
      }
### this check is useless for longitude: periodicity !
#    if (Lon1 > Lon2) warning(paste("Inconsistent Lon1=",Lon1,"Lon2=",Lon2))

    if (ggg$jScansPositively) {
      Lat1 <- ggg$latitudeOfFirstGridPointInDegrees
      Lat2 <- ggg$latitudeOfLastGridPointInDegrees
    }
    else{
      Lat2 <- ggg$latitudeOfFirstGridPointInDegrees
      Lat1 <- ggg$latitudeOfLastGridPointInDegrees
    }
    if (Lat1 > Lat2) warning(paste("Inconsistent Lat1=",Lat1,"Lat2=",Lat2))

    SW <- c(Lon1,Lat1)
    NE <- c(Lon2,Lat2)
    info <- "Rotated LatLon grid"

### ATTENTION: proj4 uses a different convention to define the rotation
### therefore the assignment seems a bit strange.
### I don't yet know how to deal with the Angle, as I haven't got example data
    SPlat <- ggg$latitudeOfSouthernPoleInDegrees
    SPlon <- ggg$longitudeOfSouthernPoleInDegrees
    SPangle <- ggg$angleOfRotationInDegrees
    if(SPangle != 0) warning("Rotated LatLon with SPangle not supported yet.")
    projection <- list(proj = "ob_tran","o_proj" = "latlong",
                       "o_lat_p" = -SPlat,"o_lon_p" = 0,"lon_0" = SPlon)
#    projection <- list(proj="rotlalo",SPlat=SPlat,SPlon=SPlon,SPangle=SPangle)
# the proj4 interface expects latlong to be in radians FOR INVERSE PROJECTION ONLY.
    RR=project(list(x=c(Lon1,Lon2)*pi/180,y=c(Lat1,Lat2)*pi/180),proj=projection,inv=T)
# Note that this returns the actual SW and NE coordinates of the 2 boundary points
# These are not equal to those in the rotated grid (which are coded into the GRIB file)
    SW <- c(RR$x[1],RR$y[1])
    NE <- c(RR$x[2],RR$y[2])

    result <- list(projection=projection,nx=nx,ny=ny,SW=SW,NE=NE,
                   dx=ggg$iDirectionIncrementInDegrees,
                   dy=ggg$jDirectionIncrementInDegrees )
    class(result) <- "geodomain"
    result

  }
### rotated mercator: no official support in grib_api yet!
#  else if(gridtype == "11"){
#     warning("Rotated Mercator projection is not officially part of the GRIB definition!")
#  }
  else if(gridtype=="reduced_gg"){
    info <- "Reduced gaussian grid (experimental!)"
    ggg <- Ginfo(gribhandle,IntPar=c("Ny","N"))
# this gives Nlon and the list of latitudes
    N <- ggg$N
    Nggg <- paste("N",N,sep="")
    data(list=Nggg,package="Rgrib2",envir=environment(NULL))
    assign("Ngg",eval(parse(text=Nggg)))
    Nlon <- Ngg$reduced

    result <- list(name=Nggg,nlon=Nlon,latlist=Ngg$latitude)
  }
#  else if(gridtype=="sh"){
#    info <- "Spectral harmonics! (experimental!)"
#  }

  else {
    info <- "Unimplemented grid"
    projection <- list(proj="unknown")
    warning(paste("Unknown grid:",gridtype))
    ggg <- Ginfo(gribhandle,
              IntPar=c("Nx","Ny",
                       "iScansNegatively","jScansPositively",
                       "jPointsAreConsecutive","alternativeRowScanning")
              )
    nx <- ggg$Nx
    ny <- ggg$Ny

    x0 <- 0
    y0 <- 0
    delx <- 1/(nx-1)
    dely <- 1/(ny-1)
    x1 <- 1
    y1 <- 1
    asp <- 1
    return(list(info=info,projection=projection,grid=gridtype,
                nx=nx,ny=ny,x0=0,y0=0,x1=1,y1=1,delx=delx,dely=dely))
  }
}
