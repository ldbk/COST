\name{convert.statsq.icesarea}
\alias{convert.statsq.icesarea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts ICES statistical rectangles to ICES areas}
\description{
Converts ICES statistical rectangles into ICES Subarea, Division or Subdivision. 
}
\usage{
convert.statsq.icesarea(statsqs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{statsqs}{A character vector of ICES statistical rectangles.}
}
\details{
Interrogates the look up table \emph{ICESAreasRects}.
}
\value{
  A list containing: 
  \item{subdivs}{The ICES subdivision.}
  \item{divs}{The ICES division.}
  \item{subarea}{The ICES subarea.}
}
\author{Alastair Pout  \email{a.pout@marlab.ac.uk}}

\seealso{\code{\link{convert.icesarea.statsq}} for the converse.}

\examples{
# the ICES areas for statistical rectangles "45F2" and "43D8"
convert.statsq.icesarea(c("45F2","43D8"))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
