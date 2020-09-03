\name{Gopen}
\alias{Gopen}
\alias{GRIB open}
\title{Open a Grib file}
\description{
  Reads the basic information of a Grib file (not the data itself).
}
\usage{
  Gopen(filename, IntPar = c(), 
    DblPar = c(), StrPar = c(),
    multi = FALSE, lextra=TRUE) 
}
\arguments{
  \item{filename}{A character string pointing at a GRIB file.}
  \item{IntPar,DblPar,StrPar}{The list of additional keys (besides the mandatory ones) to be read for all records in the file.}
  \item{multi}{If TRUE, multi-field messages are allowed.}
  \item{lextra}{If TRUE, the GRIB-1 records not recognized by grib_api are matched to an internal table with some extra table2 versions.}
}
\value{
  a data.frame of class \code{GRIBlist}, containing the basic information of the data in the file. 
 The default keys that are read for every GRIB record are
 \code{IntPar_main = c("editionNumber", "dataDate", "dataTime", 
    "validityDate", "validityTime", "Nx", "Ny",
    "table2Version", "indicatorOfParameter",
    "parameterCategory", "parameterNumber",
    "lavelType", "level"), 
    DblPar = c(), StrPar = c("shortName", "gridType")}
}

\seealso{
  \code{\link{Ginfo}}, \code{\link{Gdec}}, \code{\link{iview}}
}
\examples{\dontrun{
#Get first 5 records of a file.
mylist <- Gopen('filename')
#one grib record:
mylist[1,]
Gdec(mylist,1)
}}
\keyword{file}
