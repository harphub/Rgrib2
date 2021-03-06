\name{GRIBhandle}
\alias{GRIBhandle}
\alias{Ghandle}
\alias{GhandleList}
\alias{GhandleCount}
\alias{GhandleFree}
\alias{GhandleFreeAll}
\alias{close.GRIBhandle}
\title{GRIBhandle class}
\description{
GRIBhandle objects are pointers to a grib message that is loaded in memory.
They can be read from a file (or raw binary string) with \code{Ghandle} or created by \code{Gcreate}.
GRIBhandle objects are similar to connections in R.
They can be closed explicitely, but this is not really necessary in general,
 unless you have many open handles containing large fields.
Freeing a GRIBhandle also frees the memory that contains the GRIB message.
}
\usage{
  Ghandle(x, message=NULL, multi=FALSE, keys=list(shortName="2t"))
  GhandleList()
  GhandleCount()
  GhandleFree(gribhandle)
  GhandleFreeAll()
}
\arguments{
  \item{x}{A GRIBlist class object, the name of a GRIB file or a raw GRIB message.}
  \item{message}{An integer indicating the position in the list of the message to be opened. It may also be a list off eccodes key values. In that case, the first message that fits all keys is returned.}
  \item{multi}{Set to TRUE for experimental support of multi-message GRIB records.}
  \item{gribhandle}{A GRIBhandle}
}
\value{ \itemize{
  \item{Ghandle returns a GRIBhandle class object.}
  \item{GhandleList() returns a list of current GRIBhandles.}
  \item{GhandleCount() returns teh number of handles.}
  \item{GhandleFree() clears a GRIBhandle.}
  \item{GhandleFreeAll() clears all GRIBhandles.}
}}
\details{GRIBhandles are objects that store a pointer to some GRIB message in memory. If \code{x} is a raw vector, it is supposed to be a single GRIB message (\code{message, multi} are ignored). 
  
}
\seealso{
  \code{\link{Gopen}},\code{\link{Gmod}}, \code{\link{Gdec}},\code{\link{Gwrite}}
}
\examples{
\dontrun{
#Get 5th message in a file.
h1 <- Ghandle('filename',5)
GhandleList()
GhandleFree(h1)
}}
\keyword{file}
