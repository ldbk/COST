\name{ices.division.lines}
\alias{ices.division.lines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Adds ICES Division lines to a map}
\description{
Adds ICES Subarea and Division boundaries to an existing plot. 
}
\usage{
ices.division.lines(division = NULL, area = NULL, lty = 1, col = 1, lwd = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{division}{Optional character vector of which Divisions to do, the default is to do all.}
  \item{area}{Optional character vector of which Subareas to do, the default is to do all.}
  \item{lty}{Line type, the default is 1.}
  \item{col}{Line colour, the default is 1 (black).}
  \item{lwd}{Line width, the default is 1.}
}
\details{
If \code{area} is specified the constituent division lines are also plotted.
\cr The Baltic areas are not (yet) covered.
}
%\value{
 % ~Describe the value returned
  %If it is a LIST, use
  %\item{comp1 }{Description of 'comp1'}
  %\item{comp2 }{Description of 'comp2'}
%}

\author{Liz Clarke \email{e.d.clarke@marlab.ac.uk}}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(NHcoast)
plot(NHcoast$lon,NHcoast$lat,xlim=c(-55,25),ylim=c(35,75),type="l")
ices.division.lines(col=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
