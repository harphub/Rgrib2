\name{Ghandle}
\alias{Ghandle}
\title{Create a Grib handle from a message in a file or a sample template}
\description{
 returns a GRIBhandle object for the message.
}
\usage{
  Ghandle(x,message)
}
\arguments{
  \item{x}{The name of a GRIB file. x may also be a GRIBlist object.}
  \item{imessage}{An integer indicating the
  position of the message in the file or list. }
}
\value{
a GRIBhandle.
}
\details{ takes an object of class "GRIBhandle" as input and returns the actual data. record can also be a filename or GRIBlist object, in which case field must be specified as an integer or a name.
  
}
\seealso{
  \code{\link{Gopen}}, \code{\link{GRIBhandle}}
}
\examples{
#Get first 5 records of a file.
h1 <- Ghandle('filename',5)
#decode first record
mydata <- Gdec(h1)
}
\keyword{file}
