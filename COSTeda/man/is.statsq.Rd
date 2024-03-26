\name{is.statsq}
\alias{is.statsq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Checks for ICES statistical rectangle format}
\description{
 Function that checks its argument against the ICES statistical rectangle format. 
}
\usage{
is.statsq(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{A character vector to be checked against the ICES statistical rectangle format.}
}
\details{
  ICES statistical rectangles follow a four digit format e.g. "45F3", the first two digits representing latitude, the second two longitude. This function simply checks if x is 4 characters long, if the 1st, 2nd and 4th are numerals in the range 0 to 9, and the third is a capital letter.
Of use in distinguishing the type of spatial stratification of a COST object between statistical rectangles, ICES areas and FAO areas. }

\value{
\item{TRUE}{if the above conditions are met.}
}

\author{Alastair Pout  \email{a.pout@marlab.ac.uk}}


\section{Warning}{
This only checks the format of x, there may not necessarily be a ICES statistical rectangle with that exact name.
}
              
\seealso{{\code{code.list$rect}}, for a full list of the ICES statistical rectangles used in the COST exchange format.}
\examples{  
# 35D2 follows the ICES rectangle format
is.statsq("35D2")
# but ICES area VIa does not
is.statsq("VIa")
                                    }
                                    
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

