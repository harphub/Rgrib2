\name{Gmod}
\alias{Gmod}
\alias{GRIB modify}
\title{Modify parameters or data from a GRIB message}
\description{
  Modifies parameter entries and/or data in a GRIBhandle. 
}
\usage{
  Gmod(x,IntPar=list(),StrPar=list(),DblPar=list(),data=NULL,precision=NULL
       nbits=NULL)
}
\arguments{
  \item{x}{An object of class \code{GRIBhandle}.}
  \item{IntPar, StrPar, DblPar}{Lists (or character vectors) giving the names of integer, character (string) and numeric (double) parameters to be modified in the message(s), and the new values.}
  \item{data}{Data vector to be encoded in x.}
  \item{multi}{Logical. Setting it to TRUE allows for multi-data messages (UNTESTED!)}
  \item{precision}{Integer value for decimal precision of the GRIB packing. Alternatively, one may fix the number of bits per value.}
  \item{nbits}{Bits per value in the GRIB packing. If it is not precisied, GRIB\_API defaults to 24 bits.}
}
\value{
No return value (NULL).
}
\details{
  
}
\seealso{
  \code{\link{Gdec}}, \code{\link{Gopen}}, \code{\link{Gmod}}, \code{\link{Gwrite}}
}
\examples{\dontrun{
#Get first 5 records of a file.
mygrib <-Gopen('filename')
#return level info from all messages in the file:
a <- Ghandle(mygrib,1)
Gmod(a,IntPar=list(typeOfLevel=105,level=500),data=Gdec(a)*5)
Gwrite(a,file=newfile,append=TRUE)
}}

\keyword{file}
