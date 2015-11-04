\name{Ggrid}
\alias{Ggrid}
\alias{Gdomain}
\title{Extract domain from GRIB message}
\description{
 returns the grid specifications for a GRIBhandle. Ggrid is the old name and is obsolete. Do not use it!
}
\usage{
  Gdomain(gribhandle)
  Ggrid(gribhandle)
}
\arguments{
  \item{gribhandle}{An object of class "GRIBhandle" (or a file name or a GRIBlist, in which case field must be specified as an integer or a name.)}
}
\value{
 A \code{geodomain} class object describing the grid. The data of the grib handle is not decoded.
}
\details{ takes an object of class "GRIBhandle" as input and returns the domain specification as a \code{geodomain} class. Note that only a limited number of projections are supported.
  
}
\seealso{
  \code{\link{Gdec}},  \code{\link{Ghandle}}, \code{\link{GRIBhandle}}
}
\examples{
\dontrun{
#Get first 5 records of a file.
h1 <- Ghandle('filename',5)
#decode first record
mydata <- Gdec(h1)
}}
\keyword{file}

