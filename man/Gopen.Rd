\name{Gopen}
\alias{Gopen}
\alias{GRIB open}
\title{Open a Grib file}
\description{
  Reads the basic information of a Grib file (not the data itself).
}
\usage{
  glist <- Gopen(filename,IntPar = c("editionNumber", "dataDate", "dataTime", 
    "validityDate", "validityTime", "Nx", "Ny", "table2Version", 
    "indicatorOfParameter", "indicatorOfTypeOfLevel", "level"), 
    StrPar = c("shortName", "gridType"), DblPar = c(), multi = FALSE) 
}
\value{
  a data.frame of class "GRIBlist", containing the basic information of the data in the file. 
}
\details{
  
}
\seealso{
  \code{\link{Ginfo}}, \code{\link{Gdec}}, \code{\link{iview}}
}
\examples{
#Get first 5 records of a file.
mylist <- Gopen('filename')
#one grib record:
mylist[1,]
Gdec(mylist,1)

}
\keyword{file}
