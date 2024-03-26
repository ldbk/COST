\name{subSetGear}
\alias{subSetGear}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Subsets a COST object by gear}
\description{
 Function that subsets csData, clData or ceData by gear code.  
}
\usage{
subSetGear(costobj,gear)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{costobj}{A COST object of csDate, clData or ceData class or their validated equivalents.}
\item{gear}{Vector of gear codes. }
}
\details{
subSetGear returns all components of the csData object for the specified gear or gears. 
Gears are specified as the two or three letter codes of the fishing activity category European level 4.
e.g. "OTB" = Bottom otter trawl, "SSC" = Scottish seine, "DRB" = Boat dredge etc.  
The specified costobj must have entries for \code{$foCatEu5} in the hh table if a csData object, 
or in the cl table if a clData object.}

\value{
\item{costobj}{Sub-setted COST object.}
}

\author{Alastair Pout  \email{a.pout@marlab.ac.uk}}

\seealso{\code{\link{subSetSpp}}, \code{\link{subSetTrip}}, \code{\link{subSetProj}}, \code{\link{subSetTarget}} for other sub-setting functions.}

\examples{  
# Sub-setting the sole csData to get all "OTB" records.  
data(sole)
newsoledata <-subSetGear(sole.cs,c("OTB"))
}
                                                             
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

