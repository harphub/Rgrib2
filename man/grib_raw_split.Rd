\name{grib_raw_split}
\alias{grib_raw_split}
\alias{grib_raw_find}
\alias{raw_find}
\alias{GRIB-decode}
\title{Parse a raw GRIB message}
\description{
  Handle raw GRIB messages by identifying the location of seperate records.
}
\usage{
  grib_raw_find(msg)
  grib_raw_split(msg)
  find_in_raw(msg, pattern="GRIB")
}
\arguments{
  \item{msg}{a 'raw' vector, supposedly containing one or more GRIB records.}
  \item{pattern}{A character string (ASCII) to be searched in the raw message.}
}
\value{
  \code{find_in_raw} returns a vector of the indices in the raw message where the pattern was found. \code{grib_raw_find} returns a \code{data.frame} with columns \code{begin} and \code{end} with the indices of all seperate GRIB records. \code{grib_raw_split} returns a list of all seperate raw GRIB records.
}
\seealso{
  \code{\link{Gopen}},  \code{\link{Ghandle}}
}
\examples{
\dontrun{
#Get first 5 records of a file.
gzf <- gzfile("some_zipped_gribfile", "rb")
msg <- readBin(gzf, what="raw", n=2E7)
# decode second record:
Gdec(msg, 2)
# or
glist <- grib_raw_split(msg)
mydata <- Gdec(glist[[2]])
}}
\keyword{file}

