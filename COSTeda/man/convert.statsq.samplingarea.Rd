\name{convert.statsq.samplingarea}
\alias{convert.statsq.samplingarea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts ICES statistical rectangles to FRS sampling area codes}
\description{
Given ICES statistical rectangles this function returns the coresponding FRS sampling area codes. 
}
\usage{
convert.statsq.samplingarea(statsq,samplingarea)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{statsq}{A character vector giving the names of ICES statistical rectangle e.g. "45F2". }
  \item{samplingarea}{A character vector giving the names of sampling area; one of "Demersal", "DemersalMet", "Mackerel", "Herring", "Scallops" and "Edible crab". The default is "Demersal".}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A list containing:
  \item{parentarea}{The strata}
  \item{parentname}{The name of the strata}
  \item{statsq}{The ICES statistical rectangles}
}

\author{ Alastair Pout \email{a.pout@marlab.ac.uk}}
\seealso{\code{\link{convert.samplingarea.statsq}} for obtaining the statistical rectangles within a sampling area.}
\examples{

# Demersal sampling areas for "47F0" and "46F0"
convert.statsq.samplingarea(c("47F0","46F0"))
%# and the Nephrops areas 
%convert.statsq.samplingarea(c("47F0","46F0"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
