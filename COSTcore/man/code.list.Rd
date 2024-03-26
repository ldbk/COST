\name{code.list}
\alias{code.list}
\docType{data}
\title{Code lists of expected values for COST variables }
\description{
  Code lists of expected values for COST character string variables.
}
\usage{data(code.list)}
\format{
A list of 28 variables each a data frame with (usually) \code{$code} and \code{$description} and possibly other information. 

}
%\details{
%To do. 
%}
%\section{Warning }{To do. }

\source{
FishFrame 5.0 Exchange Format Specification  25th January 2008.
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(code.list)
# cost variables for which there are code lists i.e. all the character fields
names(code.list)
# the (start of the) expected species list
head(code.list$spp)
# (Start of the) list of ICES statistical rectangles 
head(code.list$rect)

%# maybe str(code.list) ; plot(code.list) ...
}
\keyword{datasets}
