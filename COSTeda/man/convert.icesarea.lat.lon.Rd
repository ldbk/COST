\name{convert.icesarea.lat.lon}
\alias{convert.icesarea.lat.lon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Returns the coordinates of the (approximate) centre of an ICES area }
\description{
Given an ICES Subarea, Division or Subdivision (i.e. in the form "V", "Vb", "Vb2" respectively), 
this function returns a list containing the longitude and latitude of the approximate centre of the
area. These locations are suitable for displaying numeric or character information on spatial plots.
}
\usage{
convert.icesarea.lat.lon(icesarea)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{icesarea}{A character vector giving ICES Subareas, Divisions or Subdivisions in Roman numeric format. }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A list containing:
  \item{lon}{Longitude in decimal degrees}
  \item{lat}{Latitude in decimal degrees}
}

\author{ Alastair Pout \email{a.pout@marlab.ac.uk}}
\seealso{ \code{\link{ices.division.lines}} for plotting ICES Subarea Division and Subdivision boundaries, \code{\link{convert.icesarea.statsq}} for obtaining the component statistical rectangles of an area.}
\examples{

data(NHcoast)
plot(NHcoast$lon,NHcoast$lat,xlim=c(-10,10),ylim=c(50,60),type="l")
ices.division.lines()
a <-convert.icesarea.lat.lon("IVb")
text(a$lon,a$lat,"This is ICES area IVb")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
