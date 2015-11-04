\name{Gdescribe}
\alias{Gdescribe}
\alias{Gtime}
\alias{Glevel}
\title{Extract parameter/time/level information from a GRIB message or file}
\description{
  Wrappers for Ginfo that do some further intepretation.`
}
\usage{
  Gdescribe(gribhandle)
  Gtime(gribhandle,...)
  Glevel(gribhandle,...)
}
\arguments{
  \item{gribhandle}{A \code{GRIBhandle}.}
  \item{...}{Options for Ginfo.}
}
\value{\itemize{
  \item{Gdescribe() returns a list of parameter name, origin and level.}
  \item{Gtime returns a string representation of forecast date and range.}
  \item{Glevel returns level information.}
}}
\details{All these functions are called by \code{Gdec} to extract specific information. Gdescribe uses information from grib_api. Some local GRIB-1 tables may not be known to grib_api, and they may be made accessible via the data table \code{extratab}.

Gtime and Glevel get specific information from a GRIBhandle.
}
\seealso{
  \code{\link{Ginfo}}, \code{\link{Gopen}},\code{\link{extratab}}
}
\keyword{file}
