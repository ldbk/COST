\name{ices.division.names}
\alias{ices.division.names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Adds ICES Division names to a map}
\description{
Adds ICES Subarea and Division names to an existing plot. 
}
\usage{
ices.division.names(text.cex = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.cex}{Cex size of the text}
 
}
%\details{
%}
%\value{
 % ~Describe the value returned
  %If it is a LIST, use
  %\item{comp1 }{Description of 'comp1'}
  %\item{comp2 }{Description of 'comp2'}
%}

\author{Alastair Pout \email{a.pout@marlab.ac.uk}}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(NHcoast)
plot(NHcoast$lon,NHcoast$lat,xlim=c(-55,25),ylim=c(35,75),type="l")
ices.division.lines(col=2)
ices.division.names(0.7)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
