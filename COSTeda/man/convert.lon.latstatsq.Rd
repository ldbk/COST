\name{convert.lon.lat.statsq}
\alias{convert.lon.lat.statsq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Finds the ICES statistical rectangle for a lon, lat point.}
\description{
Finds the ICES statistical rectangle that contains the given point where the point is specified as decimal lon,lat coordinates. 
}
\usage{
convert.lon.lat.statsq(lon,lat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{ Longitude in decimal degrees. }
  \item{lat}{ Latitude in decimal degrees. }
}

\value{  
  \item{}{A character vector of ICES statistical rectangle(s).}
}
\details{
Whole degree longitude increments and half degree latitude increments are rounded upwards, hence (1,60) will return statsq "49F1"
the centre of which is 1.5, 60.25.   
%  ~~ If necessary, more details than the __description__ above ~~
}
\section{Warning}{This function differs from convert.lat.lon.statsq in package "Liz" which rounds down whole degree longitude increments and half degree latitude increments.}

\author{ Liz Clarke, Alastair Pout,  \email{e.d.clarke@marlab.ac.uk}, \email{a.pout@marlab.ac.uk} }

\seealso{\code{\link{convert.statsq.lat.lon}} for the converse.}
\examples{
convert.lon.lat.statsq(c(-1,-1.56),c(65,65.3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line


