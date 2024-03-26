\name{numeric.list}
\alias{numeric.list}
\docType{data}
\title{Ranges of numeric COST variables }
\description{
  The minimum and maximun expected values of COST numeric variables.
}
\usage{data(numeric.list)}
\format{
A list of 40 variables each a numeric vector of length 2 with min and max expected value of the specified variable. 
}
%\details{
%#  Test data set only. Not to be used or cited without prior permission. 
%To do. 
%}
%\section{Warning }{To do.}

\source{
  FishFrame 5.0 Exchange Format Specification  25th January 2008. 
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(numeric.list)
# the numeric variables
names(numeric.list)
# the expected age range of a fish 
numeric.list$age
}
\keyword{datasets}
