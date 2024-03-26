\name{convert.statsq.lat.lon}
\alias{convert.statsq.lat.lon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Finds the coordinates of the centre of an ICES rectangle}
\description{
Given an ICES statistical rectangle (i.e. in the form "54E4"), 
this function returns a list containing the latitude and longitude of the centre of the
rectangle. 
}
\usage{
convert.statsq.lat.lon(statsq)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{statsq}{ A character vector giving ICES statistical rectangles, i.e. in the form "54E4". }
}

\value{
  
  \item{lon}{Logitude centre of the statistical rectangle(s) }
  \item{lat}{Latitude centre of the statistical rectangle(s) }
  
}

\author{ Liz Clarke \email{e.d.clarke@marlab.ac.uk} }

\seealso{\code{\link{convert.lon.lat.statsq}} for the converse. 
}
\examples{
convert.statsq.lat.lon(c("43E3","54D9"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line


