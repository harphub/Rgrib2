"Gdescribe" <- function(gribhandle)
{
  ## NOTE: in grib1&2, parameter.name is an alias for name
  ##                 parameterName is almost identical, but somehow can differ (even with typos)
  ##                 parameterName="Wind speed", but name="10 metre wind speed"
  ## So which should be used?
  ggg <- Ginfo(gribhandle,
          StrPar=c("centre", "subCentre", "parameterName", "levelType", "name", "units"),
          IntPar=c("level", "editionNumber", "table2Version", "indicatorOfParameter",
                   "parameterCategory", "parameterNumber")
         )
### a temporary fix for unconventional tables
  if (ggg$name=="unknown") {
    if (ggg$editionNumber==1) {
      zz <- match(paste(ggg$table2Version,ggg$indicatorOfParameter,sep="\r"),
                  with(Rgrib2::extratab,paste(table2Version,indicatorOfParameter,sep="\r")))
      if (!is.na(zz)) {
        ggg$parameterName <- as.character(Rgrib2::extratab$name[zz])
        ggg$unit <- as.character(Rgrib2::extratab$unit[zz])
      }
    } else {
      zz <- match(paste(ggg$parameterCategory, ggg$parameterNumber,sep="\r"),
                  with(Rgrib2::extratab2,paste(parameterCategory,parameterNumber,sep="\r")))
      if (!is.na(zz)) {
        ggg$parameterName <- as.character(Rgrib2::extratab2$name[zz])
        ggg$unit <- as.character(Rgrib2::extratab2$unit[zz])
      }
    }
  }
### return
  return(list(name = ggg$parameterName,
              origin = ggg$centre,
              level = ggg$level,
              leveltype = ggg$levelType,
              unit = ggg$units))
}

#####################################
Gtime <- function(gribhandle, ...)
{
# grib_api bug: gives error message if timeUnit is not "h"
# try to avoid by calling in 2 steps -> no difference
  # FIXME: grib2 does not have timeRangeIndicator (so it returns 0)
  # you need typeOfProcessedData
  ggg1 <- Ginfo(gribhandle, StrPar=c("dataDate", "dataTime", "stepUnits"))
  ggg2 <- Ginfo(gribhandle, IntPar=c("startStep", "endStep", "timeRangeIndicator"), ...)
### Initial date
  result <- list(basedate = as.POSIXct(paste(ggg1$dataDate, ggg1$dataTime),
                                        format="%Y%m%d %H%M", tz="UTC"))
  
#  result <- format(basedate, "%Y/%m/%d %H:%M")

### Is it a forecast or what...
  ### FIXME: this should also deal with accumulations (4) ...
  if (ggg2$timeRangeIndicator %in% c(0, 10)) {
    scale <- switch(ggg1$stepUnits,
                    "h"=1,
                    "m"=60,
                    "s"=3600,
                    1)
    result$leadtime <- as.numeric(ggg2$startStep)
    result$validdate <- result$basedate + result$leadtime / scale
    result$stepUnit <- ggg1$stepUnits
  } else {
    ## result$leadtime <- as.numeric(ggg2$endStep)/scale
    result$start <- ggg2$startStep
    result$end <-    ggg2$endStep
    result$stepUnit <- ggg1$stepUnits
  }
  result
}

Glevel <- function(gribhandle,...)
{
  ggg <- Ginfo(gribhandle,IntPar=c("levelType","topLevel",
            "bottomLevel"),
            StrPar=c("stepUnits"),...)
}


