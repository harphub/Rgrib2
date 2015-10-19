\name{Ginfo}
\alias{Ginfo}
\alias{Ginfo.GRIBhandle}
\alias{Ginfo.GRIBlist}
\alias{Ginfo.character}
\title{Extract information from a GRIB message or file}
\description{
  Reads a list  information of a Grib file (not the encoded data itself).
}
\usage{
  Ginfo(x,IntPar=c(),StrPar=c(),DblPar=c(),rList=NULL,multi=FALSE)
\method{Ginfo}{GRIBhandle}(x,IntPar=c(),StrPar=c(),DblPar=c())
\method{Ginfo}{GRIBlist}(x,IntPar=c(),StrPar=c(),DblPar=c(),rList=NULL,multi=FALSE)
\method{Ginfo}{character}(x,IntPar=c(),StrPar=c(),DblPar=c(),rList=NULL,multi=FALSE)
}
\arguments{
  \item{x}{An object of class \code{GRIBhandle}, a file name or a \code{GRIBlist}. In the first case, the }
  \item{IntPar, StrPar, DblPar}{Character vectors giving the names of integer, character (string) and numeric (double) parameters to be decoded from the message(s). The parameter names are as described in the GRIB_API documentation.}
  \item{rList}{An numeric vector indicating the
  position of the messages in the file. Not used if x is a GRIBhandle. The default is 1 (read only first message). If rList is NULL, all messages in the file are read.}
  \item{multi}{Logical. Setting it to TRUE allows for multi-data messages (UNTESTED!)}}
\value{
A data.frame with one column per parameter and one row per GRIB message. 
}
\details{ .
  
}
\seealso{
  \code{\link{Gdec}}, \code{\link{Gopen}}, \code{\link{Gmod}}
}
\examples{\dontrun{
#Get first 5 records of a file.
mygrib <-Gopen('filename')
#return level info from all messages in the file:
Ginfo(mygrib,IntPar=c("typeOfLevel","level"),rList=NULL)
}}
\keyword{file}
