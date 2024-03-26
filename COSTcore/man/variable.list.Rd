\name{variable.list}
\alias{variable.list}
\docType{data}
\title{Variables used within the COST data exchange format}
\description{
  A data frame of all the variables used within the COST data exchange format. 
  Culled almost verbatim from the Fish Frame exchange format document of 25 January 2008. 
}
\usage{data(variable.list)}
\format{
A data frame with 69 observations of 13 variables. 
 \describe{
    \item{\code{variable}}{The abbreviated variable name.}
    \item{\code{description}}{The full variable name.}
  \item{\code{type}}{The type of the \code{variable}: character string "s", integer "i" or real "r".}
\item{\code{ce}}{Indicator variable for presence of \emph{variable.list\$variable} in the ce table. 1 if present 0 otherwise.}    
\item{\code{cl}}{As above for the cl table.}                      %modif MM
\item{\code{tr}}{As above for the tr table.}    
\item{\code{hh}}{As above for the hh table.}    
\item{\code{sl}}{As above for the sl table.}    
\item{\code{hl}}{As above for the hl table.}    
\item{\code{ca}}{As above for the ca table.}    
\item{\code{requirement}}{Required, either mandatory M or optional O}   
\item{\code{basic.checks}}{Either numeric ranges or code.list}   
\item{\code{comments}}{More about the variable}                   %modif MM
  }

}
%\details{
%#  Test data set only. Not to be used or cited without prior permission. 
%To do. 
%}
%\section{Warning }{To do }

\source{
 FishFrame 5.0 Exchange Format Specification  25th January 2008. 
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(variable.list)
%# maybe str(variable.list) ; plot(variable.list) ...
}
\keyword{datasets}
