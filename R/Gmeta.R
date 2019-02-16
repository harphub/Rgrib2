#-------------------------------------------#
# Part of R-package Rgrib2                  #
# Copyright (c) 2003-2016 Alex Deckmyn      #
#   Royal Meteorological Institute, Belgium #
# alex.deckmyn@meteo.be                     #
# Released under GPL-3 license              #
#-------------------------------------------#

"Gdescribe" <- function(gribhandle)
{
  ggg <- Ginfo(gribhandle,
          StrPar=c("centre", "subCentre", "parameterName", "levelType", "name"),
          IntPar=c("level", "editionNumber", "table2Version", "indicatorOfParameter",
                   "parameterCategory", "parameterNumber")
         )
### a temporary fix for unconventional tables
  if (ggg$name=="unknown") {
    if (ggg$editionNumber==1) {
    zz <- match(paste(ggg$table2Version,ggg$indicatorOfParameter,sep="\r"),
                with(Rgrib2::extratab,paste(table2Version,indicatorOfParameter,sep="\r")))
    if (!is.na(zz)) ggg$parameterName <- as.character(Rgrib2::extratab$name[zz])
    } else {
    zz <- match(paste(ggg$parameterCategory, ggg$parameterNumber,sep="\r"),
                with(Rgrib2::extratab2,paste(parameterCategory,parameterNumber,sep="\r")))
    if (!is.na(zz)) ggg$parameterName <- as.character(Rgrib2::extratab2$name[zz])
    }
  }
### return
  return(list(name=ggg$parameterName,origin=ggg$centre,
              level=ggg$level,leveltype=ggg$levelType))
}

#####################################
Gtime <- function(gribhandle,...)
{
# grib_api bug: gives error message if timeUnit is not "h"
# try to avoid by calling in 2 steps -> no difference
  ggg1 <- Ginfo(gribhandle, StrPar=c("dataDate", "dataTime", "stepUnits"))
  ggg2 <- Ginfo(gribhandle, IntPar=c("startStep", "endStep", "timeRangeIndicator"), ...)
### Initial date
### this is why it's best to ask dataDate as a string, not integer
#  initdate <- as.Date(ggg$dataDate,"%Y%m%d")

  inityear  <- substring(ggg1$dataDate, 1, 4)
  initmonth <- substring(ggg1$dataDate, 5, 6)
  initday   <- substring(ggg1$dataDate, 7, 8)
  inithour  <- substring(ggg1$dataTime, 1, 2)
  initmin   <- substring(ggg1$dataTime, 3, 4)
# for backward compatibility (temporary)
  anatime   <- paste0(inityear,"/",initmonth,"/",initday," z",inithour,":",initmin)

### Is it a forecast or what...

  if (ggg2$timeRangeIndicator==10) fcrange <- paste0("+", ggg2$startStep, ggg2$stepUnits)
  else fcrange <- paste0(ggg2$startStep,"-",ggg2$endStep," ",ggg1$stepUnits)

  result <- paste(anatime, fcrange)
  attr(result, "basedate") <- as.POSIXct(anatime, format="%Y/%m/%d %H:%M", tz="UTC")
#  attr(result, "leadtime") <- fcrange * 3600
#  attr(result, "validdate") <- basedate + fcrange
  result
}

Glevel <- function(gribhandle,...)
{
  ggg <- Ginfo(gribhandle,IntPar=c("indicatorOfTypeOfLevel","topLevel",
            "bottomLevel"),
            StrPar=c("stepUnits"),...)
}


