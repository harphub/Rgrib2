\name{Gwrite}
\alias{Gwrite}
\alias{GRIB write}
\title{Write a GRIB message (handle) to a file.}
\description{
 Writes a GRIB message.
}
\usage{
  Gwrite(gribhandle,filename,append=TRUE)
}
\arguments{
  \item{gribhandle}{An object of class \code{GRIBhandle}.}
  \item{filename}{Character string giving the file name to write the message to. If the file does not yet exist, it is created. If it already exists, behaviour depends on \code{append}.}
  \item{append}{Logical. If TRUE, the message is appended to the file (if it exists). If FALSE, the complete file is overwritten.}
}
\value{
NULL 
}
\seealso{
  \code{\link{Gdec}}, \code{\link{Gopen}}, \code{\link{Gmod}}
}
\examples{\dontrun{
#Get first 5 records of a file.
mygrib <-Ghandle('filename',1)
#return level info from all messages in the file:
Gwrite(mygrib,newfile,append=TRUE)
}}
\keyword{file}
