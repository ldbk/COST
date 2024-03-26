\name{subSetTarget}
\alias{subSetTarget}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Subsets a COST object by target assemblage}
\description{
 Function that subsets csData, clData or ceData by target assemblage.  
}
\usage{
subSetTarget(costobj,assemblage)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{costobj}{A COST object of csDate, clData or ceData class or their validated equivalents.}
\item{assemblage}{Vector of target assemblage codes. }
}
\details{
subSetTarget returns all components of the csData object for the specified target assemblage codes. 
Target assemblages are specified as the final three letter code of the fishing activity category European level 5.
e.g. "DEF" = Demersal fish, "MOL" = Molluscs, "SPF" = Small pelagic fish etc. 
The specified costobj must have entries for \code{$foCatEu5} in the hh table if a csData object, 
in the cl table if a clData object or in the ce table if a ceData object.}

\value{
\item{costobj}{Sub-setted COST object.}
}

\author{Alastair Pout  \email{a.pout@marlab.ac.uk}}


\seealso{\code{\link{subSetSpp}}, \code{\link{subSetTrip}}, \code{\link{subSetProj}}, \code{\link{subSetGear}} for other sub-setting functions.}

\examples{  
# Sub-setting the sole csData to get all records for trips targeting demersal fish "DEF" and molluscs "MOL".  
data(sole)
newsoledata <-subSetTarget(sole.cs,c("DEF","MOL"))

}

                                                             
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

