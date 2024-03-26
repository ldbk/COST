\name{subSetProj}
\alias{subSetProj}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Subsets a COST object by project}
\description{
 Function that subsets csData by project code.  
}
\usage{
subSetProj(costobj,proj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{costobj}{A COST object either of csDate or csDataVal class. }
\item{proj}{Vector of proj codes. }
}
\details{
subSetProj returns all components of the csData object for the specified project or projects. }

\value{
\item{costobj}{Sub-setted COST object.}
}

\author{Alastair Pout  \email{a.pout@marlab.ac.uk}}

\seealso{\code{\link{subSetSpp}}, \code{\link{subSetTrip}}, \code{\link{subSetGear}}, \code{\link{subSetTarget}} for other sub-setting functions.}


\examples{  
# Sub-setting the sole csData to get the market sampling and observer project records.  
data(sole)
newsoledata <-subSetProj(sole.cs,c("MarkSamp","Obsmer"))
}
                                                             
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

