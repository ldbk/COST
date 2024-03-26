\name{convert.icesarea.statsq}
\alias{convert.icesarea.statsq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts ICES areas into ICES statistical rectangles}
\description{
Converts an ICES Subarea, Division or Subdivision (i.e. in the form "V", "Vb", "Vb2" respectively)
into the constituent ICES statistical rectangles.
}
\usage{
convert.icesarea.statsq(areas)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{areas}{A vector of ICES Subarea, Division or Subdivision names in Roman numeric format.}
}
\details{
Interrogates the look up table \emph{ICESAreasRects}.
}
\value{
  A list containing: 
  \item{statsq}{The constituent ICES statistical rectangles for the specified areas.}
  \item{parentarea}{A vector of the same length as \code{statsq} giving the parent ICES area name.}

}
\author{Alastair Pout  \email{a.pout@marlab.ac.uk}}

\seealso{\code{\link{ices.division.lines}} for plotting ICES Subarea, Division and Subdivision boundaries,
\code{\link{convert.statsq.lat.lon}} for obtaining the longitude and latitude centres of statistical rectangles.}

\examples{
# the statistical rectangles that make up areas VIb and VIIg
statsqs <-convert.icesarea.statsq(c("VIb","VIIg"))
# plotting them 
statsq.latlon <-convert.statsq.lat.lon(statsqs$statsq)
plot(statsq.latlon$lon,statsq.latlon$lat,pch=15,cex=1.5,xlim=c(-25,5),ylim=c(48,63))
ices.division.lines(col=2)
data(NHcoast)
lines(NHcoast$lon,NHcoast$lat)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
