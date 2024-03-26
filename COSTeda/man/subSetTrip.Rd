\name{subSetTrip}
\alias{subSetTrip}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Subsets a COST object by trip code}
\description{
 Function that subsets csData by trip.  
}
\usage{
subSetTrip(costobj,trpCode,silent=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{costobj}{A COST object either of csDate, csDataVal or csDataCons class.}
\item{trpCode}{Vector of trip codes. }
\item{silent}{Logical, if TRUE does not print summary to the screen. }
}
\details{
subSetTrip returns all components of the csData object for the specified trip or trips. }

\value{
\item{costobj}{Sub-setted COST object.}
}

\author{Alastair Pout  \email{a.pout@marlab.ac.uk}}

\seealso{\code{\link{subSetSpp}}, \code{\link{subSetProj}}, \code{\link{subSetGear}}, \code{\link{subSetTarget}} for other sub-setting functions.}

\examples{  
# Sub-setting the cod2004 FRS sampling data to get only the records for two specified trips 
data(cod2004cs)
newcoddata <-subSetTrip(cod2004cs,c("01020104A","01030104A"))

# Sub-setting the sole csData to get only the records for the first 4 trips 
data(sole)
newsoledata <-subSetTrip(sole.cs,c(1,2,3,4))
}
                                                             
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

