\name{GRIBhandle}
\alias{GRIBhandle}
\alias{GhandleList}
\alias{GhandleCount}
\alias{GhandleFree}
\alias{GhandleFreeAll}
\alias{close.GRIBhandle}
\title{GRIBhandle class}
\description{
 GRIBhandle objects are pointers to a grib message that is loaded in memory. They can be read from a file with \code{Ghandle} or created by \code{Gcreate}. GRIBhandle objects are similar to connections in R. They can be closed explicitely, but this is not really necessary in general, unless you have many open handles conatining large fields. Freeing a GRIBhandle also frees the memory that contains the GRIB message.
}
\usage{
  Ghandle(x,message)
  GhandleList()
  GhandleCount()
  GhandleFree(gribhandle)
  GhandleFreeAll()

}
\arguments{
}
\value{
}
\details{
  
}
\seealso{
  \code{\link{Ghandle}}, \code{\link{Gmod}}, \code{\link{Gcreate}},\code{\link{Gwrite}}
}
\examples{
#Get 5th message in a file.
h1 <- Ghandle('filename',5)
GhandleList()
GhandleFree(h1)
}
\keyword{file}
