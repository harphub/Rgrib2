\name{Gfind}
\alias{Gfind}
\alias{GRIB find}
\title{Locate GRIB message inside a file}
\description{
  Find the position of a particular message (field) inside a GRIB file.
}
\usage{
  Gfind(x,par="t",levelType="P",level="NULL",all=FALSE)
}
\arguments{
  \item{x}{Either a character string (filename) or a GRIBlist object. Anything that has a Ginfo method. }
  \item{par}{A character to be matched to the "shortName" field, or an integer to be matched to the parameter index. Currently, table2Version is not taken into account. Use Glocate for more detailed searches.}
  \item{levelType}{An integer according to the GRIB level type. It may also be a character similar to FA files ("P"=100,"H"=105 or "S"=107). This is only taken into account if level!=NULL.}
  \item{all}{If TRUE, all fields are returned. if FALSE, only the position(s)}
}
\value{
A vector with the message indices of all messages that satisfy the required values of the parameters.
}
\details{ 
 The function works by first calling Ginfo for the list of chosen parameters. Then the list is searched for messages that satisfy all the required values. 
}
\seealso{
  \code{\link{Glocate}}
}
\examples{
#Find T2m in a file:
Gfind('filename',"z",levelType="P",level=500)
}
\keyword{file}
