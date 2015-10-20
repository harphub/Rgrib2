\name{Gcreate}
\alias{Gcreate}
\title{Create a new GRIB handle}
\description{
  Creates a new GRIBhandle object.
}
\usage{
  Gcreate(gribformat=2,domain,sample)
}
\arguments{
  \item{gribformat}{Denotes the GRIB edition: either 1 or 2.}
  \item{domain}{A \code{geodomain} object defining the grid. If \code{domain} is not specified, the grid definition of thye sample is unmodified.}
  \item{sample}{Character string. Denotes the name of a GRIBfile to be used as a template for the new message. TIf undefined, the default is "regular_ll_sfc_gribN" where N is the gribedition.}}
\value{
A \code{GRIBhandle} for a new GRIB message.
}
\details{This command returns a \code{GRIBhandle}. This message can then be further modified using \code{Gmod}.
  
}
\seealso{
  \code{\link{GRIBhandle}}, \code{\link{Gmod}}, \code{\link{Ghandle}}
}
\examples{
\dontrun{
#Get first 5 records of a file.
mygrib <-Gopen('filename')
#return level info from all messages in the file:
Ginfo(mygrib,IntPar=c("typeOfLevel","level"),rList=NULL)
}}
\keyword{file}
