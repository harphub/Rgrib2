# FIXME:
# The chosen parameter keys can be changed,
# but some are mandatory!!!
# There is some duplication (grib 1/2)
"Gopen" <-
function (filename,
          IntPar=c(),
          DblPar=c(),
          StrPar=c(),
          multi=FALSE, lextra=TRUE,
          meta_from=1)
{
  # mandatory entries:
  # NOTE: levelType is an alias for indicatorOfTypeOfLevel AND typeOfFirstFixedSurface
  #       but there is no common alias for parameterNumber & indicatorOfParameter
  #          (parameter stands for paramId, a unique number given by ECMWF to all possible
  #           parameters that exist somewhere in an eccodes table)
  IntPar_main <- c("editionNumber",
                   "dataDate", "dataTime",
                   "validityDate", "validityTime",
                   "Nx", "Ny",
                   "table2Version", "indicatorOfParameter",
                   "parameterCategory", "parameterNumber",
                   "levelType", "level")
  DblPar_main <- c()
  StrPar_main <- c("shortName", "gridType", "units")

  IntPar <- union(IntPar_main, IntPar)
  DblPar <- union(DblPar_main, DblPar)
  StrPar <- union(StrPar_main, StrPar)

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
  ## TODO: a user should have the possibility to add table entries without
  ##       re-compiling Rgrib2. Basically, there should be a way to "override"
  ##       the eccodes shortName (even if it is not "unknown")
  ##       because in real life, there are a lot of non-complient GRIB files
  ##       *but* maybe Rgrib2 is not the place for that, it's just an eccodes interface
  ##       it could (and already can) be in e.g. harpIO
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
    zz2 <- match(with(result, paste(table2Version, indicatorOfParameter, levelType, level, sep="\r")),
                 with(Rgrib2::specialnames, paste(table2Version, indicatorOfParameter, levelType, level, sep="\r")))
    zz3 <- which(!is.na(zz2))
    if (length(zz3)>0) result$shortName[zz3] <- as.character(Rgrib2::specialnames$shortName[zz2[zz3]])

  }
### NEW: a byte index
#  cat("Adding fast index.\n")
  g_index <- .Call("Rgrib_fast_index", filename, max_msg=as.integer(2000))
  names(g_index) <- c("msg_loc", "msg_len", "msg_ed")
  g_index <- data.frame(g_index)
  if (dim(g_index)[1] != dim(result)[1]) {
#    cat("index: ", dim(g_index)[1], "list: ", dim(result)[1], "\n")
#    warning("Multi-field messages not yet supported by byte location index.")
  } else {
    result <- cbind(result, g_index)
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


# alternative version:
# first create a byte location index, then analyse the messages 1 by 1
"Gopen2" <-
function (filename,
          IntPar=c("editionNumber",
                   "dataDate", "dataTime",
                   "validityDate", "validityTime",
                   "Nx", "Ny",
                   "table2Version",
                   "indicatorOfParameter",
                   "parameterCategory", "parameterNumber",
                   "levelType", "level"),
          DblPar=c(), StrPar=c("shortName", "gridType", "units"),
          multi=FALSE, lextra=TRUE,
          meta_from=1)
{
  # get "dataDate", "dataTime", "validityDate", "validityTime", "Nx", "Ny",
  # ONLY ONCE !
  # some keys are interesting to know, but not for indexation
  # some are only for grib-2 ?
  index_keys <- c("table2Version",
                   "indicatorOfParameter",
                   "parameterCategory", "parameterNumber",
                   "levelType", "level", "shortName")

### passing a logical only works on recent installations, I think
### so passing multi as an integer is safer
  filename <- path.expand(filename)
  if (!file.exists(filename)) stop("File ", filename, " not found.")
  nmessages <- .C("Rgrib_count_messages", filename=filename, nrec=integer(1),
                 multi=as.integer(multi))$nrec

  if (is.na(nmessages)) stop("Error opening file.")

  gindex <- .Call("Rgrib_index", filename, index_keys)
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
    zz2 <- match(with(result, paste(table2Version, indicatorOfParameter, levelType, level, sep="\r")),
                 with(Rgrib2::specialnames, paste(table2Version, indicatorOfParameter, levelType, level, sep="\r")))
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



