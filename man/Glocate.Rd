\name{Glocate}
\alias{Glocate}
\alias{GRIB locate}
\title{Locate GRIB message inside a file}
\description{
  Find the position of a particular message (field) insiode a GRIB file.
}
\usage{
  Glocate(filename,IntPar=c(),StrPar=c(),DblPar=c(),...)
}
\arguments{
  \item{filename}{Either a character string (filename) or a GRIBlist object. Anything that has a Ginfo method. }
  \item{IntPar, StrPar, DblPar}{Lists of parameters and their values for the wanted field.}
  \item{...}{Options for Ginfo.}
}
\value{
A vector with the message indices of all messages that satisfy the required values of the parameters.
}
\details{ 
 The function works by first calling Ginfo for the list of chosen parameters. Then the list is searched for messages that satisfy all the required values. 
}
\seealso{
  \code{\link{Gdec}}, \code{\link{Gopen}}, \code{\link{Ginfo}}
}
\examples{
#Find T2m in a file:
Glocate('filename',StrPar=list(shortName="t2"))
}
\keyword{file}
