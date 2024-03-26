\name{dbePlotRep}
\alias{dbePlotRep}
\alias{dbePlotRep,dbeOutput-method}
\docType{methods}
\title{Graphical display of 'dbeOutput' replicates}
\description{
Method for plotting replicates data from an input 'dbeOutput' object (distribution of total estimates replicates, description of length or age structure replicates).
}

\usage{              
dbePlotRep(object,Slot,probs=c(0.05,0.95),step=NA,origin=TRUE,dispKey=TRUE,
           KurtSkew=FALSE,\dots)
}

\arguments{
  \item{object}{A \emph{dbeOutput} object with replicates information.}
  \item{Slot}{A \emph{dbeOutput} slot with a 'rep' element (eg "totalW" or "lenStruc").}
  \item{probs}{Numeric with 2 elements (or NA for no display) : quantiles defining displayed confidence intervals for each set of replicates (per stratum and length/age class). Only useful for "lenStruc" or "ageStruc" \emph{Slot} parameter}
  \item{step}{Numeric. If given, empty length or age classes will be considered and displayed, according to specified value. Only useful for "lenStruc" or "ageStruc" \emph{Slot} parameter.}
  \item{origin}{Logical. Only useful for "lenStruc" or "ageStruc" \emph{Slot} parameter. If \code{TRUE}, original raw estimates, stored in 'rep' object for \code{iter} value at 0, are displayed.} 
  \item{dispKey}{Logical. If \code{TRUE}, a legend is drawn}
  \item{KurtSkew}{Logical. If \code{TRUE}, skewness and kurtosis values are calculated for each set of replicates (per stratum and length/age class) and as specific plot is displayed. Only useful for "lenStruc" or "ageStruc" \emph{Slot} parameter.}  
  \item{...}{Further graphical arguments.}
}

\details{
This method provides 3 types of graphical outputs. If \emph{Slot} is "totalN" or "totalW", a frequency histogram of total estimates replicates is displayed. 
If \emph{Slot} is "lenStruc" or "ageStruc", then means at length/age calculated from replicates are showed with confidence interval 
(if \emph{probs} is given) and raw estimates (if \emph{origin} is TRUE). 
If \emph{KurtSkew} is TRUE, skewness and kurtosis values at length/age are calculated from replicates, and drawn.    
}

\author{Mathieu Merzereaud}
\seealso{\code{\link{dbeOutput}}
}

%\examples{
%to do 
%}
\keyword{methods}
