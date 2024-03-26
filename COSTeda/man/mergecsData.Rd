\name{mergecsData}
\alias{mergecsData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Adds parent variables to the child records of a csData COST object.}
\description{
   This function is designed as a prerequisite for plotting variables within the tr, sl, hl and ca data frames of a csData object where the spatio-temporal and technical information within the hh data frame is required. 
}
\usage{
mergecsData(csobj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{csobj}{ an object of class csData  }
}
\details{
csData objects consist of 5 nested data frames: tr, hh, sl, hl and ca. Information of ICES statistical rectangle, 
ICES (or FAO) areas, and gear type are recorded as variables only within the hh data frame.  
This function adds the variables 
\cr
\cr\$rect, \$area, \$date, \$foCatNat, \$foCatEu5, \$foCatEu6, \$quarter, \$month, and \$yearfromdate
\cr\cr to the tr, hl, sl and ca data frames and defines 
\cr\cr\$quarter, \$month, and \$yearfromdate
\cr\cr for the hh data frame. 
\cr \$yearfromdate is an additional variable to check that \$year in the tr, sl and hl data frames are 
compatible with the year derived from \$date within the hh data frame. 

% ~~ If necessary, more details than the description above ~~
}
\value{
%  ~Describe the value returned
% If it is a LIST, use
  \item{csobj}{ An amended csData object with the additional variables as outlined above}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Alastair Pout \email{a.pout@marlab.ac.uk} }
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with 
\section{Warning }{The returned csobj is no longer in csData format.}
%}
\seealso{\code{\link{spacePlot}} which calls this function for plotting csData objects.}
\examples{
%library(COSTdata)
data(cod2004cs)
# A csData@hl table contains data on length frequencies but no 
# information on where these data were obtained, when (other than the year) 
# and with what gear type
dim(cod2004cs@hl)
head(cod2004cs@hl)
newcod2004cs <-mergecsData(cod2004cs)
# The merged csData@hl table contains the additional 
# information on where these data were obtained, when and with what gear type
dim(newcod2004cs@hl)
head(newcod2004cs@hl)
}

\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
