#-------------------------------------------#
# Part of R-package Rgrib2                  #
# Copyright (c) 2003-2019 Alex Deckmyn      #
#   Royal Meteorological Institute, Belgium #
# alex.deckmyn@meteo.be                     #
# Released under GPL-3 license              #
#-------------------------------------------#

"Gopen" <-
function (filename,
          IntPar=c("editionNumber",
                   "dataDate", "dataTime",
                   "validityDate", "validityTime",
                   "Nx", "Ny",
                   "table2Version",
                   "indicatorOfParameter",
                   "parameterCategory", "parameterNumber",
                   "indicatorOfTypeOfLevel", "level"),
          DblPar=c(), StrPar=c("shortName", "gridType"),
          multi=FALSE, lextra=TRUE,
          meta_from=1)
{
  ### TODO: add an index (byte locations of records or pointer to ecCodes index)
### passing a logical only works on recent installations, I think
### so passing multi as an integer is safer
  filename <- path.expand(filename)
  if (!file.exists(filename)) stop("File ", filename, " not found.")
  nmessages <- .C("Rgrib_count_messages", filename=filename, nrec=integer(1),
                 multi=as.integer(multi))$nrec

  if (is.na(nmessages)) stop("Error opening file.")

  result <- Ginfo(filename, IntPar, DblPar, StrPar,
                  rList=as.integer(1:nmessages), multi=multi)
### a patch for tables that are missing in grib_api
##  noresult <- result[result$shortName=="unknown" & resutl$table2Version==1,]
##  if (dim(noresult)[1] > 0) {
  if (lextra) {
    if (is.element("unknown", result$shortName)) {
      missing1 <- which(result$shortName=="unknown" & result$editionNumber==1)
      missing2 <- which(result$shortName=="unknown" & result$editionNumber==2)
      if (length(missing1) > 0) {
        zz <- match(with(result[missing1,],paste(table2Version,indicatorOfParameter,sep="\r")),
                    with(Rgrib2::extratab,paste(table2Version,indicatorOfParameter,sep="\r")))
### use as.character to fix for default stringsAsFactors in data()...
        result$shortName[missing1] <- as.character(Rgrib2::extratab$shortName[zz])
## we may have created some NA's: switch them back to "unknown"
        result$shortName[which(is.na(result$shortName))] <- "unknown"
      }
      if (length(missing2) > 0) {
        zz <- match(with(result[missing2,],paste(parameterCategory, parameterNumber,sep="\r")),
                    with(Rgrib2::extratab2,paste(parameterCategory, parameterNumber,sep="\r")))
### use as.character to fix for default stringsAsFactors in data()...
        result$shortName[missing2] <- as.character(Rgrib2::extratab2$shortName[zz])
## we may have created some NA's: switch them back to "unknown"
        result$shortName[which(is.na(result$shortName))] <- "unknown"
      }
    }
# EXTRA: should we try to get "2t" etc. 
#    specialnames <- get("specialnames")
    zz2 <- match(with(result, paste(table2Version, indicatorOfParameter, indicatorOfTypeOfLevel, level, sep="\r")),
                 with(Rgrib2::specialnames, paste(table2Version, indicatorOfParameter, indicatorOfTypeOfLevel, level, sep="\r")))
    zz3 <- which(!is.na(zz2))
    if (length(zz3)>0) result$shortName[zz3] <- as.character(Rgrib2::specialnames$shortName[zz2[zz3]])

  }
###
  attr(result, "filename") <- filename
  attr(result, "nmessages") <- nmessages
  gribhandle <- Ghandle(filename, meta_from)
  attr(result, "domain") <- Gdomain(gribhandle)
  attr(result, "time") <- Gtime(gribhandle)
  GhandleFree(gribhandle)
  class(result) <- c(class(result),"GRIBlist")

  result
}


