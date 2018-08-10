\name{Gdec}
\alias{Gdec}
\alias{GRIB-decode}
\title{Decode a Grib message}
\description{
 returns the values stored in the grib record
}
\usage{
  Gdec(x, field=1, level = NULL, levelType = "P", 
       get.meta = TRUE, multi = FALSE)
}
\arguments{
  \item{x}{An object of class "GRIBhandle", a file name or a GRIBlist, in which case field must be specified as an integer or a name. x can also be a GRIB message in raw (byte) format.}
  \item{field}{An integer indicating the
  position of the field in the file or list. Not used if x is a GRIBhandle.}
  \item{level, levelType}{Possibly add a level (pressure, height above surface).}
  \item{get.meta}{Logical key to retrieve parameter and date-time information.}
  \item{multi}{Setting this to TRUE turns on support for multi-field messages in GRIB2.}
}
\value{
 A data matrix with class "geofield" and attributes time, domain and description.
}
\details{ takes an object of class "GRIBhandle" as input and returns the actual data. record can also be a filename or GRIBlist object, in which case field must be specified as an integer or a name.
  
}
\seealso{
  \code{\link{Gopen}},  \code{\link{Ghandle}}, \code{\link{GRIBhandle}}
}
\examples{
\dontrun{
#Get first 5 records of a file.
h1 <- Ghandle('filename',5)
#decode first record
mydata <- Gdec(h1)
}}
\keyword{file}

