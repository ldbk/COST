\name{subSetSpp}
\alias{subSetSpp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Subsets a COST object by species}
\description{
 Function that subsets a csData or clData by species (or taxon).  
}
\usage{
subSetSpp(costobj,spp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{costobj}{A COST object either of csDate calss or clData class or their validated equivalents.}
\item{spp}{Character string specifying the species (or taxon) to subset by. either the Scientific name or the ASFIS X3A code are accepted.}
}
\details{
spp has to be in the ASFIS list}


\value{
\item{costobj}{Sub-setted COST object.}
}

\author{Alastair Pout  \email{a.pout@marlab.ac.uk}}

\examples{  
# Assuming the data set has more than one species record which this example does not....
data(cod2004cs)
# sub-setting the cod2004 FRS sampling data for cod (this only contains cod records!)
newcod2004cs <-subSetSpp(cod2004cs,"Gadus morhua")
# or alternatively 
newcod2004cs <-subSetSpp(cod2004cs,"COD")
# and for Saithe (but there are no Saithe in this example data set) 
pok2004cs <-subSetSpp(cod2004cs,"POK")
                          }
                                    
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

